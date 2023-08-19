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

import Data.Either (fromRight, isRight)

import Data.ByteString (length)

import Test.Hspec

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

serializationSpec :: Spec
serializationSpec = describe "Compilation unit (de)serialization" $ do
    let funcs = [ helloWorld ]
    let moduleName = "mainModule"
    let meta = CU.MetaData { CU.extras = [] }
    let compUnit = CU.CompilationUnit { CU.name = moduleName, CU.metaData = meta, CU.functions = funcs }

    it "has adequate equality" $ do
        (compUnit == compUnit) `shouldBe` True -- todo wtf

    it "x == (deserialize . serialize) x" $ do
        let bytes = compilationUnitToBytes compUnit
        let fromBytesOrError = compilationUnitFromBytes bytes
        fromBytesOrError `shouldSatisfy` isRight
        let fromBytes = fromRight (error "not right") fromBytesOrError
        compUnit `shouldBe` fromBytes

main :: IO ()
main = do
    mapM_ hspec [serializationSpec]
