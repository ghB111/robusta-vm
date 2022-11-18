{-# LANGUAGE NamedFieldPuns #-}

module Execution ( )
where

import Control.Monad.State.Lazy

import Types
import Vm ( Vm (..) )
import Instruction ( Instruction(..) )
import Function ( Frame(..) )

import System.IO.Unsafe ( unsafePerformIO )

{-
    Performs one instruction. 
    Can change state of function (pc and variables)
-}
performInstruction :: Instruction -> State Frame ()
performInstruction Nop  = return ()
performInstruction (Goto to) = do
    frame <- get
    put (frame {pc = to})
performInstruction IAdd = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 + x2
    put $ frame { stack = (wrap r):xs }
performInstruction IMul = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x1 * x2
    put $ frame { stack = (wrap r):xs }
performInstruction ISub = do
    frame <- get
    let ((IntV x1):(IntV x2):xs) = stack frame
    let r = x2 - x1
    put $ frame { stack = (wrap r):xs }
performInstruction INeg = do
    frame <- get
    let ((IntV x):xs) = stack frame
    put $ frame { stack = wrap (-x):xs }
performInstruction IConst0 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (0 :: Int) : stack }
performInstruction IConst1 = do
    frame@Frame{stack} <- get
    put $ frame { stack = wrap (1 :: Int) : stack }
performInstruction IDiv = do
    frame@Frame{stack} <- get
    let ( (IntV x1) : (IntV x2) : xs ) = stack
    let r = x2 `mod` x1
    put $ frame { stack = wrap r : xs }
    -- | IfICmpEq { gotoDest :: Int }
    -- | IfICmpGe { gotoDest :: Int }
    -- | IfICmpGt { gotoDest :: Int }
    -- | IfICmpLe { gotoDest :: Int }
    -- | IfICmpLt { gotoDest :: Int }
    -- | IfICmpNe { gotoDest :: Int }
    -- | IfEq     { gotoDest :: Int }
    -- | IfGt     { gotoDest :: Int }
    -- | IfLe     { gotoDest :: Int }
    -- | IfLt     { gotoDest :: Int }
    -- | IfNe     { gotoDest :: Int }
    -- | ILoad    { idx :: Int }
    -- | IStore   { idx :: Int }
    -- | InvokeF  { functionName :: String }
    -- | IReturn
    -- | Return
    -- | Ldc
    -- | Dup
    -- | Pop
    -- | Swap

