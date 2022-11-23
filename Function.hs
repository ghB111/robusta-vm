module Function
    ( Function(..)
    , Frame(..)
    , Stack
    ) where

import Types ( Value(..), Type(..) )
import Instruction

type Stack = [Value]
type Variables = [Value]

{-
    Arguements to function are passed as variables
-}
data Frame = Frame
    { variables :: [Value]
    , pc :: Int
    , stack :: Stack
    } deriving (Show, Read)


data Function = Function { name :: String
                         , argTypes :: [Type]
                         , returnType :: Type
                         , instructions :: [Instruction]
                         } deriving (Show, Read)

exFunction = Function "examples.ex" [IntT] VoidT []
