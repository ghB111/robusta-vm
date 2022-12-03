{-# LANGUAGE NamedFieldPuns #-}

module Stdlib ( stdlib
              )
where

import Control.Monad.State

import Vm 
import Function
import Types
import Instruction
import Execution
import Dsl

vmPrintLn = NativeFunction "std/println" $ do
    frame@Frame{stack} <- get
    let (ArrayV arr) = head stack
    let stringToPrint = map toChar arr
    liftIO $ putStrLn stringToPrint
    put frame { stack = tail stack }
        where toChar :: Value -> Char
              toChar (CharV ch) = ch
              toChar _          = error "expected CharV"

stdlib :: [Function]
stdlib = [ vmPrintLn ]
