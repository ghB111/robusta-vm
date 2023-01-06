{-# LANGUAGE NamedFieldPuns #-}

module Execution ( runVm )
where

import Control.Monad.State
import Data.List

import Types
import Vm ( Vm (..) )
import Instruction (  Instruction(..), FrameInstruction(..), SpecialInstruction(..) )
import Function ( Frame(..), Stack, Function(..) )

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/
replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

-- (others, stack1, stack2,... stackN) => (stack1, stack2, ... stackN)
copyNFromStack :: Int -> Stack -> [Value]
copyNFromStack n = reverse . take n

-- throws if not IntV
getIntV :: Value -> Int
getIntV (IntV x) = x
getIntV _        = error "not IntV"

performIntegerComparisonBranch :: Int -> [Ordering] -> State Frame ()
performIntegerComparisonBranch gotoDest orderings = do
    frame@Frame{stack, pc} <- get
    let [x1, x2] = map getIntV $ copyNFromStack 2 stack
    let cmpRes = compare x1 x2
    let newPc = if cmpRes `elem` orderings then gotoDest else pc
    put frame{ stack = drop 2 stack, pc = newPc}

performBranch :: Int -> [Ordering] -> State Frame ()
performBranch gotoDest orderings = do
    frame@Frame{stack, pc} <- get
    let [x] = map getIntV $ copyNFromStack 1 stack
    let cmpRes = compare x 0
    let newPc = if cmpRes `elem` orderings then gotoDest else pc
    put frame{ stack = drop 1 stack, pc = newPc}
    


{-
    Performs one frame instruction. 
    Can change state of frame (ex. pc, variables)
-}
performFrameInstruction :: FrameInstruction -> State Frame ()
performFrameInstruction Nop  = return ()
performFrameInstruction (Goto to) = do
    frame <- get
    put (frame {pc = to})

performFrameInstruction IAdd = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 + x2
    put $ frame { stack = wrap r : xs }

performFrameInstruction IMul = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 * x2
    put $ frame { stack = wrap r : xs }

performFrameInstruction ISub = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x2 - x1
    put $ frame { stack = wrap r : xs }

performFrameInstruction INeg = do
    frame <- get
    let ((IntV x):xs) = stack frame
    put $ frame { stack = wrap (-x):xs }

performFrameInstruction IConst0 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (0 :: Int) : stack }

performFrameInstruction IConst1 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (1 :: Int) : stack }

performFrameInstruction IDiv = do
    frame@Frame{stack} <- get
    let ( (IntV x1) : (IntV x2) : xs ) = stack
    let r = x2 `div` x1
    put $ frame { stack = wrap r : xs }

performFrameInstruction (IfICmpEq gotoDest) = performIntegerComparisonBranch gotoDest [EQ]
performFrameInstruction (IfICmpGe gotoDest) = performIntegerComparisonBranch gotoDest [GT, EQ]
performFrameInstruction (IfICmpGt gotoDest) = performIntegerComparisonBranch gotoDest [GT]
performFrameInstruction (IfICmpLe gotoDest) = performIntegerComparisonBranch gotoDest [LT, EQ]
performFrameInstruction (IfICmpLt gotoDest) = performIntegerComparisonBranch gotoDest [LT]
performFrameInstruction (IfICmpNe gotoDest) = performIntegerComparisonBranch gotoDest [LT, GT]

performFrameInstruction (IfEq gotoDest) = performBranch gotoDest [EQ]
performFrameInstruction (IfGt gotoDest) = performBranch gotoDest [GT]
performFrameInstruction (IfLe gotoDest) = performBranch gotoDest [LT, EQ]
performFrameInstruction (IfLt gotoDest) = performBranch gotoDest [LT]
performFrameInstruction (IfNe gotoDest) = performBranch gotoDest [LT, GT]

performFrameInstruction (ILoad idx) = do
    frame@Frame{stack, variables} <- get
    let x = variables !! idx
    put frame { stack = x : stack }

performFrameInstruction (IStore idx) = do
    -- This is a funny one, because unlike JVM, we do not have
    -- information about amount of variables a method needs. So we basically
    -- have to lazily allocate space for new variables
    -- NOTE variables do not have static type, it can change
    frame@Frame{stack, variables} <- get
    let x = head stack
    let trustedVariables = makeSureFits idx variables
    put frame { stack = drop 1 stack, variables = replace trustedVariables idx x }
    where makeSureFits :: Int -> [Value] -> [Value]
          makeSureFits idx vars
            | allocateN > 0   = vars ++ take allocateN $ repeat (VoidV ())
            where allocateN = succ idx - length vars
          makeSureFits _ vars = vars

performFrameInstruction (Ldc value) = do
    frame@Frame{stack} <- get
    put frame { stack = value : stack }

performFrameInstruction Dup = do
    frame@Frame{stack} <- get
    put frame { stack = head stack : stack }

performFrameInstruction Pop = do
    frame@Frame{stack} <- get
    put frame { stack = tail stack }

performFrameInstruction Swap = do
    frame@Frame{stack} <- get
    let [x1, x2] = copyNFromStack 2 stack
    put frame { stack = x1 : x2 : drop 2 stack }

performFrameInstruction NewArr = do
    frame@Frame{stack} <- get
    let (IntV newArrLen) = head stack
    let newArr = wrap $ replicate newArrLen () -- an array is initialized with voids
    put frame { stack = newArr : tail stack }

-- as arrays are value type, array does not
-- get deleted from the stack
performFrameInstruction ArrLen = do
    frame@Frame{stack} <- get
    let (ArrayV arr) = head stack
    let arrLen = length arr
    put frame { stack = wrap arrLen : stack }

performFrameInstruction ArrLoad = do
    frame@Frame{stack} <- get
    let [ArrayV arr, IntV idx] = copyNFromStack 2 stack
    let extractedElement = arr !! idx
    put frame { stack = extractedElement : tail stack }

performFrameInstruction ArrStore = do
    frame@Frame{stack} <- get
    let [ArrayV arr, IntV idx, value] = copyNFromStack 3 stack
    let patchedArr = ArrayV $ replace arr idx value
    put frame { stack = patchedArr : drop 3 stack }

performSpecialInstruction :: SpecialInstruction -> StateT Vm IO ()
performSpecialInstruction (InvokeF functionName) = do
    vm@Vm{frames, functions} <- get
    let currentFrame = head frames
    let matchedFuncs = filter (\x -> name x == functionName) functions
    if null matchedFuncs then error $ "function name " ++ functionName ++ " not found" else do
      let func = head matchedFuncs
      case func of
          NativeFunction{realFunc}                          -> do
              (_, newFrame) <- lift $ runStateT realFunc currentFrame
              let newFrames = newFrame : tail frames
              put vm { frames = newFrames }
          func@Function{argTypes, returnType, instructions} -> makeFrame
              where makeFrame = do
                      -- todo check arg types?
                      let argN = length argTypes
                      let args = copyNFromStack argN (stack currentFrame)
                      let currentStack = stack currentFrame
                      let currentFrameFixed = currentFrame { stack = drop argN currentStack }
                      let newFrame = Frame { variables = args , pc = 0, stack = [], function = func }
                      let newFrames = newFrame : currentFrameFixed : tail frames
                      put vm { frames = newFrames }
    
-- To be fair, this wouldn't be any different for an AReturn or LReturn
performSpecialInstruction IReturn = do
    vm@Vm{frames} <- get
    let currentFrame = head frames
    let currentStack = stack currentFrame
    let valueToReturn = head currentStack
    let prevFrame@Frame{stack=prevStack} = frames !! 1
    let prevFramePatched = prevFrame { stack = valueToReturn : prevStack }
    let newFrames = prevFramePatched : drop 2 frames
    put vm { frames = newFrames }

performSpecialInstruction Return = do
    vm@Vm{frames} <- get
    let newFrames = tail frames
    put vm { frames = newFrames }

incPc :: State Vm ()
incPc = do
    vm@Vm{frames} <- get
    let currentFrame@Frame{pc} = head frames
    let newFrame = currentFrame { pc = succ pc }
    put vm { frames = newFrame : tail frames }

performHeapInstruction :: Instruction -> State (Frame, Heap) ()
performHeapInstruction = undefined

performInstruction :: Instruction -> StateT Vm IO ()
performInstruction (FrameInstructionC instruction) = do
    curState <- get
    put $ execState incPc curState
    vm@Vm{frames} <- get
    let currentFrame@Frame{pc} = head frames
    let (_, newFrame) = runState (performFrameInstruction instruction) currentFrame
    put vm { frames = newFrame : tail frames }

performInstruction (SpecialInstructionC instruction) = do
    curState <- get
    put $ execState incPc curState
    performSpecialInstruction instruction

performInstruction (HeapInstructionC instruction) = do
    curState <- get
    put $ execState incPc curState
    vm@Vm{frames, heap} <- get
    let currentFrame@Frame{pc} = head frames
    let (_, (newFrame, newHeap)) = runState (performHeapInstruction instruction) (currentFrame, heap)
    put vm { frames = newFrame : tail frames, heap = newHeap }

-- this only takes non-native function in consideration, because putting native
-- functions in frame does not make sense
vmStep :: Vm -> IO Vm
vmStep vm@Vm{frames = curFrame@Frame{pc, function = Function{instructions}} : _} = nextVm
    where nextVm = execStateT (performInstruction nextInstruction) vm
          nextInstruction = instructions !! pc

{-
    Runs vm until it has returned to its original frame, which
    is supposed to have return code on top as return value
-}
runVm :: Vm -> IO Int
runVm coldVm = runToMain coldVm  >>= runTilOneFrame
    where runToMain :: Vm -> IO Vm -- it is supposed that main is second frame
          runToMain vm@Vm{frames} | length frames == 2 = return vm
          runToMain vm                                 = vmStep vm >>= runToMain
          runTilOneFrame :: Vm -> IO Int
          runTilOneFrame vm@Vm{frames = [resFrame]}    = return returnCode
            where (IntV returnCode) = (head . stack) resFrame
          runTilOneFrame vm                            = vmStep vm >>= runTilOneFrame
