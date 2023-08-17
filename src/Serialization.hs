{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Serialization (
    compilationUnitToBytes,
    compilationUnitFromBytes
)
where

import qualified Data.Word8 as W8

import Instruction
import Function
import CompilationUnit
import Types
import Data.Maybe (fromJust, mapMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import qualified Data.Int

import Data.Char (ord)
import Data.List (elemIndex)

import Data.ProtoLens
import qualified Proto.Robusta as Proto
import qualified Proto.Robusta_Fields as Fields
import Lens.Micro

import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Data.Bifunctor (bimap, Bifunctor (first))
-- import qualified Proto.CompilationUnit as Proto


compilationUnitToBytes :: CompilationUnit -> ByteString
compilationUnitToBytes CompilationUnit{CompilationUnit.name, metaData, functions} = encodeMessage msg
    where msg :: Proto.CompilationUnit
          msg = defMessage
                    & Fields.name .~ pack name
                    & Fields.metaData .~ metaDataToMsg metaData
                    & Fields.functions .~ map (functionToMsg constants) nonNativeFunctions
                    & Fields.constants .~ map constantToMsg constants
          constants = makeStringTable functions
          nonNativeFunctions = filter isNotNative functions -- todo subject to change to support user native funcs?
          isNotNative (NativeFunction{}) = False
          isNotNative _ = True

data Constant = ByteConstant ByteString
              | StringConstant String
              | CharConstant Char
              | IntConstant Int
              | VoidConstant ()
              deriving (Eq, Show)

constantToMsg :: Constant -> Proto.Constant
constantToMsg (ByteConstant byteString) = defMessage & Fields.byteContent .~ byteString
constantToMsg (VoidConstant ()) = defMessage & Fields.voidContent .~ defMessage
constantToMsg (IntConstant i) = defMessage & Fields.intContent .~ fromIntegral i
constantToMsg (CharConstant c) = defMessage & Fields.charContent .~ (fromIntegral $ ord c :: Data.Int.Int32)
constantToMsg (StringConstant sc) = defMessage & Fields.stringContent .~ pack sc


makeStringTable :: [Function] -> [Constant]
makeStringTable = foldl (\acc x -> merge acc $ getConstants x) []
    where merge :: [Constant] -> [Constant] -> [Constant]
          merge l r = l ++ filter (`notElem` l) r

getConstants :: Function -> [Constant]
getConstants NativeFunction{} = []
getConstants Function{instructions} = mapMaybe retrieveInstructionString instructions

valueToConstant :: Value -> Constant
valueToConstant (VoidV ()) = VoidConstant ()
valueToConstant (IntV i) = IntConstant i
valueToConstant (CharV c) = CharConstant c
valueToConstant (ArrayV _) = error "Array reference can not be constant"

retrieveInstructionString :: Instruction -> Maybe Constant
retrieveInstructionString (SpecialInstructionC (InvokeF methodName)) =
    Just $ StringConstant methodName
retrieveInstructionString (FrameInstructionC (Ldc constantValue)) = Just $ valueToConstant constantValue
retrieveInstructionString _ = Nothing

metaDataToMsg :: MetaData -> Proto.MetaData
metaDataToMsg MetaData{extras} = defMessage
    & Fields.extras .~ Map.fromList (map (bimap pack pack) extras)

functionToMsg :: [Constant] -> Function -> Proto.Function
functionToMsg _ NativeFunction{} = error "Can not serialize native function"
functionToMsg constants Function{Function.name, argTypes, returnType, instructions} = defMessage
    & Fields.name .~ pack name
    & Fields.argTypes .~ map typeToMsg argTypes
    & Fields.returnType .~ typeToMsg returnType
    & Fields.instructions .~ instructionsToBytes constants instructions

typeToMsg :: Type -> Proto.Type
typeToMsg VoidT = Proto.VoidT
typeToMsg IntT = Proto.IntT
typeToMsg CharT = Proto.CharT
typeToMsg ArrayT = Proto.ArrayT

instructionsToBytes :: [Constant] -> [Instruction] -> ByteString
instructionsToBytes constants instructions =
    ByteString.pack $ concatMap (instructionToBytes constants) instructions

instructionToBytes :: [Constant] -> Instruction -> [W8.Word8]
instructionToBytes constants instruction = case instruction of
    (SpecialInstructionC (InvokeF methodName)) -> byConstantLookup $ StringConstant methodName
    (FrameInstructionC (Ldc constant)) -> byConstantLookup $ valueToConstant constant
    _ -> [ getInstructionCode instruction ]
    where byConstantLookup constant = [ getInstructionCode instruction, fromIntegral constIdx ]
            where constIdx = fromJust $ elemIndex constant constants

-- equality is reference-agnostic for this type
newtype InstructionType = InstructionType { instruction :: Instruction }

instance Eq InstructionType where
    (InstructionType (SpecialInstructionC (InvokeF _))) == (InstructionType (SpecialInstructionC (InvokeF _))) = True
    (InstructionType (FrameInstructionC (Ldc _))) == (InstructionType (FrameInstructionC (Ldc _))) = True
    (InstructionType l) == (InstructionType r) = l == r

getInstructionCode :: Instruction -> InstructionCode
getInstructionCode specInst@(SpecialInstructionC{}) = forceLookup (InstructionType specInst) $ map (first (InstructionType . SpecialInstructionC)) specialCodes
getInstructionCode heapInst@(HeapInstructionC{}) = forceLookup (InstructionType heapInst) $ map (first (InstructionType . HeapInstructionC)) heapCodes
getInstructionCode frameInst@(FrameInstructionC{}) = forceLookup (InstructionType frameInst) $ map (first (InstructionType . FrameInstructionC)) frameCodes


forceLookup :: Eq a => a -> [(a, b)] -> b
forceLookup key = fromJust . lookup key

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
