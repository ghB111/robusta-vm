{-# LANGUAGE NamedFieldPuns #-}

module Execution ( )
where

import Control.Monad.State.Lazy
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
copyNFromStack n = reverse . (take n)

-- throws if not IntV
getIntV :: Value -> Int
getIntV (IntV x) = x
getIntV _        = error "not IntV"

performIntegerComparisonBranch :: Int -> [Ordering] -> State Frame ()
performIntegerComparisonBranch gotoDest orderings = do
    frame@Frame{stack, pc} <- get
    let [x1, x2] = map getIntV $ copyNFromStack 2 stack
    let cmpRes = compare x1 x2
    let newPc = if any (==cmpRes) orderings then gotoDest else pc
    put frame{ stack = drop 2 stack, pc = newPc}

performBranch :: Int -> [Ordering] -> State Frame ()
performBranch gotoDest orderings = do
    frame@Frame{stack, pc} <- get
    let [x] = map getIntV $ copyNFromStack 1 stack
    let cmpRes = compare x 0
    let newPc = if any (==cmpRes) orderings then gotoDest else pc
    put frame{ stack = drop 1 stack, pc = newPc}
    


{-
    Performs one frame instruction. 
    Can change state of frame (ex. pc and variables)
-}
performInstruction :: FrameInstruction -> State Frame ()
performInstruction Nop  = return ()
performInstruction (Goto to) = do
    frame <- get
    put (frame {pc = to})

performInstruction IAdd = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 + x2
    put $ frame { stack = (wrap r):xs }

performInstruction IMul = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 * x2
    put $ frame { stack = (wrap r):xs }

performInstruction ISub = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x2 - x1
    put $ frame { stack = (wrap r):xs }

performInstruction INeg = do
    frame <- get
    let ((IntV x):xs) = stack frame
    put $ frame { stack = wrap (-x):xs }

performInstruction IConst0 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (0 :: Int) : stack }

performInstruction IConst1 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (1 :: Int) : stack }

performInstruction IDiv = do
    frame@Frame{stack} <- get
    let ( (IntV x1) : (IntV x2) : xs ) = stack
    let r = x2 `mod` x1
    put $ frame { stack = wrap r : xs }

performInstruction (IfICmpEq gotoDest) = performIntegerComparisonBranch gotoDest [EQ]
performInstruction (IfICmpGe gotoDest) = performIntegerComparisonBranch gotoDest [GT, EQ]
performInstruction (IfICmpGt gotoDest) = performIntegerComparisonBranch gotoDest [GT]
performInstruction (IfICmpLe gotoDest) = performIntegerComparisonBranch gotoDest [LT, EQ]
performInstruction (IfICmpLt gotoDest) = performIntegerComparisonBranch gotoDest [LT]
performInstruction (IfICmpNe gotoDest) = performIntegerComparisonBranch gotoDest [LT, GT]

performInstruction (IfEq gotoDest) = performBranch gotoDest [EQ]
performInstruction (IfGt gotoDest) = performBranch gotoDest [GT]
performInstruction (IfLe gotoDest) = performBranch gotoDest [LT, EQ]
performInstruction (IfLt gotoDest) = performBranch gotoDest [LT]
performInstruction (IfNe gotoDest) = performBranch gotoDest [LT, GT]

performInstruction (ILoad idx) = do
    frame@Frame{stack, variables} <- get
    let x = getIntV $ variables !! idx
    put frame { stack = wrap x : stack }

performInstruction (IStore idx) = do
    -- This is a funny one, because unlike JVM, we do not have
    -- information about amount of variables a method needs. So we basically
    -- have to lazily allocate space for new variables
    -- NOTE variables do not have static type, it can change
    frame@Frame{stack, variables} <- get
    let [x] = map getIntV $ copyNFromStack 1 stack
    let trustedVariables = makeSureFits idx variables
    put frame { stack = drop 1 stack, variables = replace variables idx $ wrap x }
    where makeSureFits :: Int -> [Value] -> [Value]
          makeSureFits idx vars
            | allocateN > 0   = vars ++ (take allocateN $ repeat (VoidV ()))
            where allocateN = succ idx - (length vars)
          makeSureFits _ vars = vars

performInstruction (Ldc value) = do
    frame@Frame{stack} <- get
    put frame { stack = value : stack }

performInstruction Dup = do
    frame@Frame{stack} <- get
    put frame { stack = head stack : stack }

performInstruction Pop = do
    frame@Frame{stack} <- get
    put frame { stack = tail stack }

performInstruction Swap = do
    frame@Frame{stack} <- get
    let [x1, x2] = copyNFromStack 2 stack
    put frame { stack = x1 : x2 : drop 2 stack }

performSpecialInstruction :: SpecialInstruction -> State Vm ()
performSpecialInstruction (InvokeF functionName) = do
    vm@Vm{frames, functions} <- get
    let func@Function{argTypes, returnType, instructions}
          = head $ filter (\x -> name x == functionName) functions
    -- todo check arg types?
    let argN = length argTypes
    let currentFrame = head frames
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
    let prevFrame@{prevStack} = frames !! 1
    let prevFramePatched = prevFrame { stack = valueToReturn : prevStack }
    let newFrames = prevFramePatched : drop 2 frames
    put vm { frames = newFrames }

performSpecialInstruction Return = do
    vm@Vm{frames} <- get
    let newFrames = tail frames
    put vm { frames = newFrames }
