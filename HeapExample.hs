module HeapExample where

import Vm 
import Function
import Types
import Instruction
import Execution
import Robusta
import Dsl


-- takes 0 arguements
-- returns an array that has numbers 0-3
functionThatReturnsArray :: Function
functionThatReturnsArray = Function { name = "makeArray"
                                    , argTypes = []
                                    , returnType = ArrayT
                                    , instructions = instructions }
    where instructions = [ ldcWi 4 -- 4(array size)
                         , newArr -- arrRef()
                         , dup -- arrRef(), arrRef()
                         , ldcWi 0 -- arrRef(), arrRef(), 0
                         , ldcWi 0 -- arrRef(), arrRef(), 0
                         , arrStore -- arrRef(0)
                         , dup -- arrRef(0), arrRef(0)
                         , ldcWi 1 -- arrRef(0), arrRef(0), 1
                         , ldcWi 1 -- arrRef(0), arrRef(0), 1
                         , arrStore -- arrRef(0, 1)
                         , dup
                         , ldcWi 2
                         , ldcWi 2
                         , arrStore
                         , dup
                         , ldcWi 3
                         , ldcWi 3
                         , arrStore
                         , iRet ]

mainFuncExample :: Function
mainFuncExample = Function { name = "main"
                             , argTypes = []
                             , returnType = IntT
                             , instructions = instructions }
    where instructions = [ invokeF "makeArray" -- arrRef(0, 1, 2, 3)
                         , dup
                         , invokeF "std/println"
                         , ldcWi 0
                         , iRet ]

exVm :: Vm
exVm = stdVmWith [mainFuncExample, functionThatReturnsArray]
    where stdVmWith newFuncs = stdVm { functions = functions stdVm ++ newFuncs }

main = do
    returnCode <- runVm exVm
    print returnCode
