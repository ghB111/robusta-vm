module Serialization
where

import qualified Data.ByteString as B
import qualified Data.Word8 as W8

import Instruction

toBytes :: Instruction -> [Word8]
toBytes = undefined

{-
 If bytestring starts with a valid instruction,
 returns instruction and its length in bytes

 Otherwise, returns Nothing
-}
fromBytesPeek :: [Word8] -> Maybe (Instruction, Int)
fromBytesPeek = undefined

fromBytesOne :: [Word8] -> Maybe (Instruction, [Word8])
fromBytesOne = undefined

fromByteString :: ByteString -> Maybe ([Instruction], [Word8])
fromBytesOne = undefined


-- data Instruction
--     = SpecialInstructionC SpecialInstruction
--     | FrameInstructionC FrameInstruction
--     | HeapInstructionC HeapInstruction
--     deriving (Show, Read)

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
