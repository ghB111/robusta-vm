module Function ( Function(..) )
where

import Control.Monad.State.Lazy

import Types ( Value(..), Type(..) )

data FunctionState
    = FunctionState { variables :: [Value] }
type Variables = [Value]

performInstruction :: State FunctionState FunctionState
performInstruction

-- instance Functor FunctionState where
--     fmap = undefined

data Instruction = Instruction
    deriving (Show, Read)

data Function = Function { name :: String
                         , argTypes :: [Type]
                         , returnType :: Type
                         , instructions :: [Instruction]
                         } deriving (Show, Read)

exFunction = Function "examples.ex" [IntT] VoidT []


