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

{- Expects two main integer args. Returns their sum as return code -}
sumTwoIntegers :: Function
sumTwoIntegers = Function { name = "main"
                          , argTypes = [ArrayT]
                          , returnType = IntT
                          , instructions = mainInstructions }
    where mainInstructions =  [ iLoad 0
                              , iLoad 1
                              , invokeF "std/atoi"
                              , swap
                              , invokeF "std/atoi"
                              , iAdd
                              , iRet ]

main :: IO ()
main = do
    let vm = stdVm { functions = helloWorld : stdlib }
    returnCode <- runVm vm
    putStrLn $ "Vm return code: " ++ show returnCode

exArgs :: IO ()
exArgs = do
    let vm = (stdVmArgs ["42", "10"]) { functions = sumTwoIntegers : stdlib }
    returnCode <- runVm vm
    putStrLn $ "Vm return code: " ++ show returnCode

