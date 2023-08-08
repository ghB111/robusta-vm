{-# LANGUAGE DuplicateRecordFields #-}

module Instruction ( Instruction(..)
                   , SpecialInstruction(..)
                   , FrameInstruction(..)
                   , HeapInstruction(..) )
where

import Types ( Value(..) )

data Instruction
    = SpecialInstructionC SpecialInstruction
    | FrameInstructionC FrameInstruction
    | HeapInstructionC HeapInstruction
    deriving (Show, Read)

data SpecialInstruction
    = AReturn
    | Return
    | InvokeF  { functionName :: String }
    deriving (Show, Read)

data HeapInstruction
    = ArrLen
    | ArrNew
    | ArrLoad
    | ArrStore
    deriving (Show, Read)

data FrameInstruction
    = Nop
    | Add
    | Sub
    | Mul
    | Div
    | Neg
    | Const0
    | Const1
    | Goto     { gotoDest :: Int }
    | IfCmpEq  { gotoDest :: Int }
    | IfCmpGe  { gotoDest :: Int }
    | IfCmpGt  { gotoDest :: Int }
    | IfCmpLe  { gotoDest :: Int }
    | IfCmpLt  { gotoDest :: Int }
    | IfCmpNe  { gotoDest :: Int }
    | IfEq     { gotoDest :: Int }
    | IfGt     { gotoDest :: Int }
    | IfLe     { gotoDest :: Int }
    | IfLt     { gotoDest :: Int }
    | IfNe     { gotoDest :: Int }
    | VarLoad    { idx :: Int }
    | VarStore   { idx :: Int }
    | Ldc      { value :: Value } -- arrayV probably would not work here
    | Dup
    | Pop
    | Swap
    deriving (Show, Read)
