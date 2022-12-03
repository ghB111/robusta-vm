module Main
where

import Vm 
import Function
import Types
import Instruction
import Execution
import Robusta
import Stdlib
import Dsl

helloWorld :: Function
helloWorld = Function { name = "main"
                      , argTypes = [ArrayT]
                      , returnType = IntT
                      , instructions = mainInstructions }
    where mainInstructions =  [ ldcW $ length str, newArr ] 
                           ++ loadStrInstr
                           ++ [ invokeF "std/println", ldcW (0 :: Int), iRet ]
          str = "Hello world!"
          strIndexed :: [(Int, Char)]
          strIndexed = zip [0..] str
          loadStrInstr :: [Instruction]
          loadStrInstr = concat [ [ldcW idx, ldcW ch, arrStore] | (idx, ch) <- strIndexed ]

main :: IO ()
main = do
    let vm = stdVm { functions = helloWorld : stdlib }
    returnCode <- runVm vm
    putStrLn $ "Vm return code: " ++ show returnCode

