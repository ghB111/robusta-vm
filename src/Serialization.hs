{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Serialization (
    compilationUnitToBytes,
    compilationUnitFromBytes
)
where

import qualified Data.ByteString as B
import qualified Data.Word8 as W8

import Instruction
import Function
import CompilationUnit
import Types
import Data.Maybe (fromJust, isNothing)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

-- import qualified Proto.Robusta as Proto
-- import qualified Proto.Robusta_Fields as ProtoFields
import Data.ProtoLens
import qualified Proto.Robusta as Proto
import qualified Proto.Robusta_Fields as Fields
import Lens.Micro

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Data.Bifunctor (bimap)
-- import qualified Proto.CompilationUnit as Proto


compilationUnitToBytes :: CompilationUnit -> ByteString
compilationUnitToBytes CompilationUnit{CompilationUnit.name, metaData, functions} = encodeMessage msg
    where msg :: Proto.CompilationUnit
          msg = defMessage
                    & Fields.name .~ pack name
                    & Fields.metaData .~ metaDataToMsg metaData
                    & Fields.functions .~ map (functionToMsg stringTable) functions
                    & Fields.stringTable .~ stringTable
          stringTable = makeStringTable functions

type StringTable = [ByteString]

makeStringTable :: [Function] -> StringTable
makeStringTable = foldl (\acc x -> merge acc $ getStrings x) []
    where merge = undefined

getStrings :: Function -> StringTable
getStrings NativeFunction{} = []
getStrings Function{instructions} = map []

metaDataToMsg :: MetaData -> Proto.MetaData
metaDataToMsg MetaData{extras} = defMessage
    & Fields.extras .~ Map.fromList (map (bimap pack pack) extras)

functionToMsg :: Function -> Proto.Function
functionToMsg NativeFunction{} = error "Can not serialize native function"
functionToMsg Function{Function.name, argTypes, returnType, instructions} = defMessage
    & Fields.name .~ pack name
    & Fields.argTypes .~ map typeToMsg argTypes
    & Fields.returnType .~ typeToMsg returnType
    & Fields.instructions .~ instructionsToBytes instructions

typeToMsg :: Type -> Proto.Type
typeToMsg VoidT = Proto.VOID_T
typeToMsg IntT = Proto.IntT
typeToMsg CharT = Proto.CharT
typeToMsg ArrayT = Proto.ArrayT

instructionsToBytes :: [Instruction] -> ByteString
instructionsToBytes instructions = ByteString.pack $ concatMap instructionToBytes instructions

instructionToBytes :: Instruction -> [Word8]
instructionToBytes (SpecialInstructionC inst) = undefined

compilationUnitFromBytes :: ByteString -> Maybe CompilationUnit
compilationUnitFromBytes = undefined

type InstructionCode = W8.Word8
specialCodes :: [(SpecialInstruction, InstructionCode)]
specialCodes =
    [ (AReturn, 1)
    , (Return, 2)
    , (InvokeF "", 3) ]

heapCodes :: [(HeapInstruction, InstructionCode)]
heapCodes =
    [ (ArrLen, 4)
    , (ArrNew, 5)
    , (ArrLoad, 6)
    , (ArrStore, 7) ]

frameCodes :: [(FrameInstruction, InstructionCode)]
frameCodes =
    [ (Nop, 8)
    , (Add, 9)
    , (Sub, 10)
    , (Mul, 11)
    , (Div, 12)
    , (Neg, 13)
    , (Const0, 14)
    , (Const1, 15)
    , (Goto 0, 16)
    , (IfCmpEq 0, 17)
    , (IfCmpGe 0, 18)
    , (IfCmpGt 0, 19)
    , (IfCmpLe 0, 20)
    , (IfCmpLt 0, 21)
    , (IfCmpNe 0, 22)
    , (IfEq    0, 23)
    , (IfGt    0, 24)
    , (IfLe    0, 25)
    , (IfLt    0, 26)
    , (IfNe    0, 27)
    , (VarLoad 0, 28)
    , (VarStore 0, 29)
    , (Ldc $ makeDefault VoidT, 30)
    , (Dup, 31)
    , (Pop, 32)
    , (Swap, 33)
    ]




-- toBytes (SpecialInstructionC SpecialInstruction)
-- toBytes (FrameInstructionC FrameInstruction)
-- toBytes (HeapInstructionC HeapInstruction)



-- data SpecialInstruction
--     = AReturn
--     | Return
--     | InvokeF  { functionName :: String }
--     deriving (Show, Read)

-- data HeapInstruction
--     = ArrLen
--     | ArrNew
--     | ArrLoad
--     | ArrStore
--     deriving (Show, Read)

-- data FrameInstruction
--     = Nop
--     | Add
--     | Sub
--     | Mul
--     | Div
--     | Neg
--     | Const0
--     | Const1
--     | Goto     { gotoDest :: Int }
--     | IfCmpEq  { gotoDest :: Int }
--     | IfCmpGe  { gotoDest :: Int }
--     | IfCmpGt  { gotoDest :: Int }
--     | IfCmpLe  { gotoDest :: Int }
--     | IfCmpLt  { gotoDest :: Int }
--     | IfCmpNe  { gotoDest :: Int }
--     | IfEq     { gotoDest :: Int }
--     | IfGt     { gotoDest :: Int }
--     | IfLe     { gotoDest :: Int }
--     | IfLt     { gotoDest :: Int }
--     | IfNe     { gotoDest :: Int }
--     | VarLoad    { idx :: Int }
--     | VarStore   { idx :: Int }
--     | Ldc      { value :: Value }
--     | Dup
--     | Pop
--     | Swap
--     deriving (Show, Read)
