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
    where mainInstructions = [ iLoad 0 -- argsArray
                             , ldcW (0 :: Int) -- argsArray, 0
                             , arrLoad -- argsArray, arg0
                             , iStore 1 -- argsArray
                             , ldcW (1 :: Int) -- argsArray, 1
                             , arrLoad -- argsArray, arg1
                             , iLoad 1 -- argsArray, arg1, arg0
                             , invokeF "std/atoi" -- argsArray, arg1, arg0Int
                             , swap -- argsArray, arg0Int, arg1
                             , invokeF "std/atoi" -- argsArray, arg0Int, arg1Int
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

