module Serialization ()
where

import qualified Data.ByteString as B
import qualified Data.Word8 as W8

import Instruction
import Function
import CompilationUnit
import Types
import Data.Maybe (fromJust, isNothing)

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
