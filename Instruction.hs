{-# LANGUAGE DuplicateRecordFields #-}

module Instruction ( Instruction(..), SpecialInstruction(..), FrameInstruction(..) )
where

import Types ( Value(..) )

data Instruction
    = SpecialInstructionC SpecialInstruction
    | FrameInstructionC FrameInstruction
    deriving (Show, Read)

data SpecialInstruction
    = IReturn
    | Return
    | InvokeF  { functionName :: String }
    deriving (Show, Read)

data FrameInstruction
    = Nop
    | IAdd
    | ISub
    | IMul
    | IDiv
    | INeg
    | IConst0
    | IConst1
    | Goto     { gotoDest :: Int }
    | IfICmpEq { gotoDest :: Int }
    | IfICmpGe { gotoDest :: Int }
    | IfICmpGt { gotoDest :: Int }
    | IfICmpLe { gotoDest :: Int }
    | IfICmpLt { gotoDest :: Int }
    | IfICmpNe { gotoDest :: Int }
    | IfEq     { gotoDest :: Int }
    | IfGt     { gotoDest :: Int }
    | IfLe     { gotoDest :: Int }
    | IfLt     { gotoDest :: Int }
    | IfNe     { gotoDest :: Int }
    | ILoad    { idx :: Int }
    | IStore   { idx :: Int }
    | Ldc      { value :: Value }
    | Dup
    | Pop
    | Swap
    | ArrLen
    | NewArr
    | ArrLoad
    | ArrStore
    deriving (Show, Read)
