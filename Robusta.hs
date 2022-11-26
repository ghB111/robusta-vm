module Robusta (baseVm, baseFrame, baseFunction, baseFunctionInstructions)
where

import Vm 
import Function
import Types
import Instruction
import Execution

baseVm :: Vm
baseVm = Vm { frames = [baseFrame], functions = [] }

baseFrame :: Frame
baseFrame = Frame { variables = [], pc = 0, stack = [], function = baseFunction }

baseFunction :: Function
baseFunction = Function { name = "bootstrap"
                        , argTypes = [StringT]
                        , returnType = IntT
                        , instructions = baseFunctionInstructions }

baseFunctionInstructions :: [Instruction]
baseFunctionInstructions = [ (SpecialInstructionC $ InvokeF "main")
                           , (SpecialInstructionC IReturn) ]
