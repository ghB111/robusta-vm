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

makeNative :: String -> NativeFunctionT -> Function
makeNative name f = NativeFunction ("std/" ++ name) f

vmPrintLn = makeNative "println" $ do
    frame@Frame{stack} <- get
    let arr = head stack
    let stringToPrint = toString arr
    liftIO $ putStrLn stringToPrint
    put frame { stack = tail stack }

-- could be marked as pure
vmAtoi = makeNative "atoi" $ do
    frame@Frame{stack} <- get
    let arr = head stack
    let stringToParse = toString arr
    let resInt = (read stringToParse) :: Int
    put frame { stack = wrap resInt : tail stack }

stdlib :: [Function]
stdlib = [ vmPrintLn, vmAtoi ]

-- utils
toString :: Value -> String
toString (ArrayV arr) = map toChar arr
toString _            = error "toString conversion failed. Expected ArrayT"

toChar :: Value -> Char
toChar (CharV ch) = ch
toChar _          = error "expected CharV"
