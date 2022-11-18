{-# LANGUAGE DuplicateRecordFields #-}

module Instruction ( Instruction(..) )
where

data Instruction
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
    | InvokeF  { functionName :: String }
    | IReturn
    | Return
    | Ldc
    | Dup
    | Pop
    | Swap
    deriving (Show, Read)
