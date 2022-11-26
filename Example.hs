module Example
where

import Vm 
import Function
import Types
import Instruction
import Execution
import Robusta


ithFibFunction :: Function
ithFibFunction = Function { name = "util/fib"
                          , argTypes = [IntT]
                          , returnType = IntT
                          , instructions = instructions }
    where instructions = [ FrameInstructionC $ ILoad 0 -- load arg on stack
                         , FrameInstructionC $ Dup
                         , FrameInstructionC $ Ldc $ wrap (0 :: Int) -- base case 1
                         , FrameInstructionC $ IfICmpNe 5
                         , SpecialInstructionC $ IReturn -- return 0, 0th fib
                         , FrameInstructionC $ Dup
                         , FrameInstructionC $ Ldc $ wrap (1 :: Int) -- base case 1
                         , FrameInstructionC $ IfICmpNe 9
                         , SpecialInstructionC $ IReturn -- return 1, 1th fib
                         , FrameInstructionC $ Dup
                         , FrameInstructionC $ IConst1
                         , FrameInstructionC $ ISub
                         , SpecialInstructionC $ InvokeF "util/fib" -- get i-1
                         , FrameInstructionC $ IStore 1
                         , FrameInstructionC $ Ldc $ wrap (2 :: Int)
                         , FrameInstructionC $ ISub
                         , SpecialInstructionC $ InvokeF "util/fib" -- get i-2
                         , FrameInstructionC $ ILoad 1
                         , FrameInstructionC $ IAdd
                         , SpecialInstructionC $ IReturn ]

-- calculates i-th fib number
mainFuncExample :: Int -> Function
mainFuncExample i = Function { name = "main"
                             , argTypes = []
                             , returnType = IntT
                             , instructions = instructions i }
    where instructions i = [ FrameInstructionC $ Ldc $ wrap i
                           , SpecialInstructionC $ InvokeF "util/fib"
                           , SpecialInstructionC $ IReturn ]

fibVm :: Int -> Vm
fibVm i = baseVm { functions = [mainFuncExample i, ithFibFunction] }

-- will print i-th fib number, calculating it from the vm
main = do
    putStrLn "Print index of a fib number to calculate"
    i <- readLn :: IO Int
    print $ runVm $ fibVm i
