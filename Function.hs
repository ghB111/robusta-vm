{-# LANGUAGE NamedFieldPuns #-}

module Function
    ( Function(..)
    , Frame(..)
    , Stack
    , exFunction
    , exNativeFunction
    ) where

import Control.Monad.State.Lazy
import Debug.Trace

import Types ( Value(..), Type(..) )
import Instruction

type Stack = [Value]

{-
    Arguments to function are passed as variables
-}
data Frame = Frame
    { variables :: [Value]
    , pc :: Int
    , stack :: Stack
    , function :: Function
    } deriving (Show, Read)


type NativeFunctionT = State Frame ()
data Function = Function { name :: String
                         , argTypes :: [Type]
                         , returnType :: Type
                         , instructions :: [Instruction]
                         }
              | NativeFunction { name :: String
                               , realFunc :: NativeFunctionT
                               }
              
-- todo
instance Show Function where
    show Function{name} = "function " ++ name
    show NativeFunction{name} = "native function " ++ name

-- todo
instance Read Function where
    readsPrec _ = undefined

exFunction = Function "examples.ex" [IntT] VoidT []
exNativeFunction = NativeFunction "examples.nativeEx" $ do
    state <- get
    traceShow state $ put state
