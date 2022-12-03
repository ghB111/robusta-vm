module Robusta ( baseVm
               , stdVm )
where

import Vm 
import Function
import Types
import Instruction
import Execution
import Dsl

import Stdlib

{-
    Vm with a bootstrap frame. This vm  you can lauch, provided you add a main function to it 
    The frame cannot be reentered, i.e. there are no exported functions at all
-}
baseVm :: Vm
baseVm = Vm { frames = [baseFrame], functions = [] }

{-
    Base vm with standard library installed
-}
stdVm = baseVm { functions = stdlib }

baseFrame :: Frame
baseFrame = Frame { variables = [], pc = 0, stack = [], function = baseFunction }

baseFunction :: Function
baseFunction = Function { name = "bootstrap"
                        , argTypes = [VoidT] -- it does not really get any arguements
                        , returnType = IntT -- return value is return code
                        , instructions = baseFunctionInstructions }

baseFunctionInstructions :: [Instruction]
baseFunctionInstructions = [ invokeF "main"
                           , iRet ]
