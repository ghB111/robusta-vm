import Vm 
import Function
import Types
import Instruction
import Execution
import Robusta
import Stdlib
import Dsl
import Serialization
import qualified CompilationUnit as CU

import Data.ByteString (length)

helloWorld :: Function
helloWorld = Function { name = "main"
                      , argTypes = [ArrayT]
                      , returnType = IntT
                      , instructions = mainInstructions }
    where mainInstructions =  ldcString "Hello World!"
                           ++ [ invokeF "std/println", ldcW (0 :: Int), aRet ]

{- Expects two main integer args. Returns their sum as return code -}
sumTwoIntegers :: Function
sumTwoIntegers = Function { name = "main"
                          , argTypes = [ArrayT]
                          , returnType = IntT
                          , instructions = mainInstructions }
    where mainInstructions = [ varLoad 0 -- argsArray
                             , ldcW (0 :: Int) -- argsArray, 0
                             , arrLoad -- argsArray, arg0
                             , varStore 1 -- argsArray
                             , ldcW (1 :: Int) -- argsArray, 1
                             , arrLoad -- argsArray, arg1
                             , varLoad 1 -- argsArray, arg1, arg0
                             , invokeF "std/atoi" -- argsArray, arg1, arg0Int
                             , swap -- argsArray, arg0Int, arg1
                             , invokeF "std/atoi" -- argsArray, arg0Int, arg1Int
                             , add
                             , aRet ]

main :: IO ()
main = do
    let funcs = helloWorld : stdlib
    let moduleName = "mainModule"
    let meta = CU.MetaData { CU.extras = [] }
    let compUnit = CU.CompilationUnit { CU.name = moduleName, CU.metaData = meta, CU.functions = funcs }
    print compUnit
    let shouldBeOk = compUnit == compUnit
    print shouldBeOk
    let bytes = compilationUnitToBytes compUnit
    putStrLn $ "Serialized size " ++ show (Data.ByteString.length bytes)
    let fromBytesOrError = compilationUnitFromBytes bytes
    either (\err -> do
        putStrLn $ "Error " ++ err
        ) (\fromBytes -> do
        putStrLn $ "Got name: " ++ CU.name fromBytes
        print fromBytes
        let onlyFunc = head (CU.functions fromBytes)
        print onlyFunc
        let insns = instructions onlyFunc
        print insns
        let ok = fromBytes == compUnit
        -- let ok = True
        let msg = if ok then "OK" else "FAIL"
        putStrLn $ "Result: " ++ msg
            ) fromBytesOrError
