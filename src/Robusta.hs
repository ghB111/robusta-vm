module Robusta ( baseVm
               , baseVmArgs
               , stdVmArgs
               , stdVm )
where

import Vm 
import Function
import Types
import Instruction
import Execution
import Dsl

import Stdlib

import Heap

{-
    Vm with a bootstrap frame. This vm  you can lauch, provided you add a main function to it 
    The frame cannot be reentered, i.e. there are no exported functions at all
-}
baseVm :: Vm
baseVm = baseVmArgs []

baseVmArgs :: [String] -> Vm
baseVmArgs args = Vm { frames = [baseFrame args], functions = [], heap = makeEmptyHeap }

{-
    Base vm with standard library installed
-}
stdVm :: Vm
stdVm = stdVmArgs []

stdVmArgs :: [String] -> Vm
stdVmArgs args = (baseVmArgs args) { functions = stdlib }

baseFrame :: [String] -> Frame
baseFrame args = Frame { variables = [], pc = 0, stack = [], function = baseFunction args }

baseFunction :: [String] -> Function
baseFunction args = Function { name = "bootstrap"
                             , argTypes = [VoidT] -- it does not really get any arguements
                             , returnType = IntT -- return value is return code
                             , instructions = baseFunctionInstructions args }

baseFunctionInstructions :: [String] -> [Instruction]
baseFunctionInstructions args =  [ldcW $ length args, arrNew]
                              ++ loadAllStringsToArr
                              ++ [ invokeF "main" ]
                              ++ [ aRet ]
    where indexedArgs :: [(Int, [Instruction])]
          indexedArgs = zip [0..] $ map ldcString args
          loadAllStringsToArr :: [Instruction]
          loadAllStringsToArr = concat [ (ldcW idx : instr) ++ [arrStore]  | (idx, instr) <- indexedArgs ]

