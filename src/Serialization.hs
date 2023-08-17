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
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Data.ByteString (ByteString, null)
import qualified Data.ByteString as ByteString

import qualified Data.Int

import Data.Char (ord, chr)
import Data.List (elemIndex)
import Control.Monad (msum)

import Data.ProtoLens ( encodeMessage, decodeMessage, Message(defMessage) )
import qualified Proto.Robusta as Proto
import qualified Proto.Robusta_Fields as Fields
import Lens.Micro

import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Data.Bifunctor (bimap, Bifunctor (first))
import Data.Map (toList)
import Data.Tuple (swap)
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

decodeType :: Proto.Type -> Type
decodeType Proto.VoidT = VoidT
decodeType Proto.IntT = IntT
decodeType Proto.CharT = CharT
decodeType Proto.ArrayT = ArrayT
decodeType (Proto.Type'Unrecognized v) = error $ "unrecoginzed type coded " ++ show v

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

compilationUnitFromBytes :: ByteString -> Either String CompilationUnit
compilationUnitFromBytes byteString =
    let decoded :: Either String Proto.CompilationUnit
        decoded = decodeMessage byteString
    in fmap compilationUnitFromMessage decoded

compilationUnitFromMessage :: Proto.CompilationUnit -> CompilationUnit
compilationUnitFromMessage protoUnit =
    let unitName :: String
        unitName = protoUnit ^. Fields.name . to unpack
        metaData :: MetaData
        metaData = protoUnit ^. Fields.metaData . to decodeMetaData
        constants :: [Constant]
        constants = protoUnit ^. Fields.constants & each %~ decodeConstant
        functions = protoUnit ^. Fields.functions & each %~ decodeFunction constants
    in CompilationUnit { CompilationUnit.name = unitName
                       , metaData = metaData
                       , functions = functions }

decodeConstant :: Proto.Constant -> Constant
decodeConstant protoConstant =
    let maybeConstant = protoConstant ^. Fields.maybe'content
        constantRef :: Proto.Constant'Content
        constantRef = fromMaybe (error "Bad constant type") maybeConstant
    in case constantRef of
        Proto.Constant'ByteContent byteString -> ByteConstant byteString
        Proto.Constant'VoidContent _ -> VoidConstant ()
        Proto.Constant'IntContent integer -> IntConstant $ fromIntegral integer
        Proto.Constant'CharContent charAsInt -> CharConstant . chr $ fromIntegral charAsInt
        Proto.Constant'StringContent str -> StringConstant $ unpack str

decodeFunction :: [Constant] -> Proto.Function -> Function
decodeFunction constants protoFunction =
    let instructionBytes = protoFunction ^. Fields.instructions . to (decodeInstructions constants)
        name = protoFunction ^. Fields.name . to unpack
        argTypes :: [Type]
        argTypes = protoFunction ^. Fields.argTypes & each %~ decodeType
        returnType = protoFunction ^. Fields.returnType . to decodeType
    in Function{ Function.name = name
                , argTypes = argTypes
                , instructions = instructionBytes
                , returnType = returnType }

decodeInstructions :: [Constant] -> ByteString -> [Instruction]
decodeInstructions constants byteString = decodeInstructions' (ByteString.unpack byteString) []
    where decodeInstructions' ::  [W8.Word8] -> [Instruction] -> [Instruction]
          decodeInstructions' bytes acc =
            if ByteString.null byteString then reverse acc else
                let (decoded, decodedLength) = decodeOne bytes
                in decodeInstructions' (drop decodedLength bytes) (decoded : acc)
          decodeOne :: [W8.Word8] -> (Instruction, Int)
          decodeOne [] = error "can not decode empty bytestring"
          decodeOne (code : otherBytes) =
            let maybeNoRefInstruction = makeInstructionByCode code
                refId = fromIntegral $ head otherBytes
                referredConstant = constants !! refId
            in case maybeNoRefInstruction of
                (SpecialInstructionC (InvokeF _)) -> (SpecialInstructionC (InvokeF funcName), 2)
                    where (StringConstant funcName) = referredConstant
                (FrameInstructionC (Ldc _)) -> (FrameInstructionC (Ldc value), 2)
                    where value = case referredConstant of
                                    (StringConstant _) -> error "wtf"
                                    (IntConstant int) -> IntV int
                                    (ByteConstant byte) -> IntV $ fromIntegral $ head $ ByteString.unpack byte -- todo this is subject to change to raw data
                                    (CharConstant charConstant) -> CharV charConstant
                                    (VoidConstant _) -> VoidV ()
                _ -> (maybeNoRefInstruction, 1)



makeInstructionByCode :: W8.Word8 -> Instruction
makeInstructionByCode code =
    let specialTable = map swap specialCodes
        heapTable = map swap heapCodes
        frameTable = map swap frameCodes
        maybeSpecial = SpecialInstructionC <$> lookup code specialTable
        maybeHeap = HeapInstructionC <$> lookup code heapTable
        maybeFrame = FrameInstructionC <$> lookup code frameTable
    in fromMaybe (error "") $ msum [maybeSpecial, maybeHeap, maybeFrame]

decodeMetaData :: Proto.MetaData -> MetaData
decodeMetaData protoMetaData =
    let extras :: [(String, String)]
        extras = protoMetaData ^. Fields.extras . to toList . to (map $ bimap unpack unpack)
    in MetaData extras

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
