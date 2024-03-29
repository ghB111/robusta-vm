{-# LANGUAGE NamedFieldPuns #-}

module Function
    ( Function(..)
    , Frame(..)
    , Stack
    , exFunction
    , exNativeFunction
    , NativeFunctionT
    ) where

import Control.Monad.State
    ( StateT, MonadIO(liftIO), MonadState(put, get) )

import Types ( Value(..), Type(..) )
import Instruction
import Heap hiding (get, put)

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


type NativeFunctionT = StateT (Frame, Heap) IO ()
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

instance Eq Function where
    NativeFunction{name=nameL} == NativeFunction{name=nameR} = nameL == nameR
    Function{ name=nameL
            , argTypes=argTypesL
            , returnType=returnTypeL
            , instructions=instructionsL } == Function{ name=nameR
                                                      , argTypes=argTypesR
                                                      , returnType=returnTypeR
                                                      , instructions=instructionsR } 
                                                      = nameL == nameR && argTypesL == argTypesR && returnTypeL == returnTypeR && instructionsL == instructionsR
    _ == _ = False


exFunction :: Function
exFunction = Function "examples.ex" [IntT] VoidT []

exNativeFunction :: Function
exNativeFunction = NativeFunction "examples.nativeEx" $ do
    state <- get
    liftIO $ putStrLn "We now have side-effects"
    put state
