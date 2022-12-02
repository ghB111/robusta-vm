module Example
where

import Vm 
import Function
import Types
import Instruction
import Execution
import Robusta
import Dsl


ithFibFunction :: Function
ithFibFunction = Function { name = "util/fib"
                          , argTypes = [IntT]
                          , returnType = IntT
                          , instructions = instructions }
    where instructions = [ iLoad 0 -- load arg on stack
                         , dup
                         , ldc $ wrap (0 :: Int) -- base case 0
                         , ifICmpNe 5
                         , iReturn -- return 0, 0th fib
                         , dup
                         , ldc $ wrap (1 :: Int) -- base case 1
                         , ifICmpNe 9
                         , iReturn -- return 1, 1th fib
                         , dup
                         , iConst1
                         , iSub
                         , invokeF "util/fib" -- get i-1
                         , iStore 1
                         , ldc $ wrap (2 :: Int)
                         , iSub
                         , invokeF "util/fib" -- get i-2
                         , iLoad 1
                         , iAdd
                         , iReturn ]

-- calculates i-th fib number
mainFuncExample :: Int -> Function
mainFuncExample i = Function { name = "main"
                             , argTypes = []
                             , returnType = IntT
                             , instructions = instructions i }
    where instructions i = [ ldc $ wrap i
                           , invokeF "util/fib"
                           , iReturn ]

fibVm :: Int -> Vm
fibVm i = baseVm { functions = [mainFuncExample i, ithFibFunction] }

-- will print i-th fib number, calculating it from the vm
main = do
    putStrLn "Print index of a fib number to calculate"
    i <- readLn :: IO Int
    print $ runVm $ fibVm i
