module Serialization ( fromBytesPeek
                     , fromBytesOne
                     , fromByteString )
where

import qualified Data.ByteString as B
import qualified Data.Word8 as W8
import Data.Tuple ( swap )
import Data.ByteString.Lazy ( ByteString )
import Data.Text ()
import Data.Word8 (Word8)
import Data.Serialize ( putWord8, Serialize(..), Putter )
import Data.Char (ord)

import Instruction
import Function
import CompilationUnit
import Types
import Data.Maybe (fromJust, isNothing)
import qualified Data.Bifunctor

{-
 If bytestring starts with a valid instruction,
 returns instruction and its length in bytes

 Otherwise, returns Nothing
-}
fromBytesPeek :: [Word8] -> Maybe (Instruction, Int)
fromBytesPeek = undefined

fromBytesOne :: [Word8] -> Maybe (Instruction, [Word8])
fromBytesOne = undefined

-- todo this probably should report errors
fromByteString :: ByteString -> Either String [Instruction]
fromByteString = undefined

toWord8 :: String -> [Word8]
toWord8 = map (fromIntegral . ord)

toBytesUnit :: CompilationUnit -> [Word8]
toBytesUnit cu@CompilationUnit{metaData, functions} = 
    let magic = toWord8 "ROBU"
        header = 
    in magic ++ header 


toBytes :: Instruction -> [Word8]
toBytes = undefined

instance Serialize CompilationUnit where
    put CompilationUnit{metaData, functions} = do
        put metaData
        put functions
    get = do
        metaData <- get
        functions <- get
        return $ CompilationUnit { metaData, functions }

instance Serialize MetaData where
    put MetaData{unitName, extras} = do
        put unitName
        put extras
    get = do
        unitName <- get
        extras <- get
        return $ MetaData {unitName, extras}

instance Serialize Function where
    put NativeFunction{} = error "cannot serialize a native function"
    put Function{name, argTypes, returnType, instructions} = do
        put name
        put argTypes
        put returnType
        put instructions

    get = do
        name <- get
        argTypes <- get
        returnType <- get
        instructions <- get
        return $ Function {name, argTypes, returnType, instructions}

typeEncoding :: [(Type, Word8)]
typeEncoding =
    [ (VoidT, 0)
    , (IntT, 1)
    , (CharT, 2)
    , (ArrayT, 3) ]

instance Serialize Type where
    put t = putWord8 $ fromJust $ lookup t typeEncoding
    get = do
        typeEncoded <- get
        let t = lookup typeEncoded encodedToType
        maybe (fail $ "bad encoded type: " ++ show typeEncoded) return t
            where encodedToType = map swap typeEncoding

instance Serialize Instruction where
    put (SpecialInstructionC inst) = putSpecial inst
    put (FrameInstructionC inst) = putFrame inst
    put (HeapInstructionC inst) = putHeap inst
    get = undefined

putSpecial :: Putter SpecialInstruction
putSpecial inst = undefined
putFrame :: Putter FrameInstruction
putFrame = undefined
putHeap :: Putter HeapInstruction
putHeap = undefined

type InstructionCode = Word8
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
