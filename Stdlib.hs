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

import qualified Heap
import Heap hiding (get, put)

makeNative :: String -> NativeFunctionT -> Function
makeNative name = NativeFunction ("std/" ++ name)

vmPrintLn = makeNative "println" $ do
    (frame@Frame{stack}, heap) <- get
    let (ArrayV arrRef) = head stack
    let arr = Heap.get heap arrRef
    let stringToPrint = toString arr
    liftIO $ putStrLn stringToPrint
    put (frame { stack = tail stack }, heap)

-- could be marked as pure
vmAtoi = makeNative "atoi" $ do
    (frame@Frame{stack}, heap) <- get
    let (ArrayV arrRef) = head stack
    let arr = Heap.get heap arrRef
    let stringToParse = toString arr
    let resInt = read stringToParse :: Int
    put (frame { stack = wrap resInt : tail stack }, heap)

vmTrace = makeNative "debug/trace" $ do
    state <- get
    liftIO $ print state

stdlib :: [Function]
stdlib = [ vmPrintLn, vmAtoi, vmTrace ]

-- utils
toString :: [Value] -> String
toString arr@((CharV _):_) = map toChar arr
toString arr = show arr

toChar :: Value -> Char
toChar (CharV ch) = ch
toChar _          = error "expected CharV"
