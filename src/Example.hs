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
    where instructions = [ varLoad 0 -- load arg on stack
                         , dup
                         , ldc $ wrap (0 :: Int) -- base case 0
                         , ifCmpNe 5
                         , aRet -- return 0, 0th fib
                         , dup
                         , ldc $ wrap (1 :: Int) -- base case 1
                         , ifCmpNe 9
                         , aRet -- return 1, 1th fib
                         , dup
                         , const1
                         , sub
                         , invokeF "util/fib" -- get i-1
                         , varStore 1
                         , ldc $ wrap (2 :: Int)
                         , sub
                         , invokeF "util/fib" -- get i-2
                         , varLoad 1
                         , add
                         , aRet ]

-- calculates i-th fib number
mainFuncExample :: Int -> Function
mainFuncExample i = Function { name = "main"
                             , argTypes = []
                             , returnType = IntT
                             , instructions = instructions i }
    where instructions i = [ ldc $ wrap i
                           , invokeF "util/fib"
                           , aRet ]

fibVm :: Int -> Vm
fibVm i = baseVm { functions = [mainFuncExample i, ithFibFunction] }

nativeVm = baseVm { functions = [exNativeFunction, main] }
    where main :: Function
          main = Function { name = "main"
                          , argTypes = []
                          , returnType = IntT
                          , instructions = mainInstructions }
          mainInstructions :: [Instruction]
          mainInstructions = [ invokeF "examples.nativeEx"
                             , ldc $ wrap (0 :: Int)
                             , aRet ]

-- this will print "we now have side-effects"
-- as a result of a native function call
main = do
    returnCode <- runVm nativeVm
    print returnCode
