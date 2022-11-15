module Function ( Function(..) )
where

import Types ( Type(..) )

data Instruction = Instruction

data Function = Function { name :: String
                         , argTypes :: [Type]
                         , returnType :: Type
                         , instructions :: [Instruction]
                         }
                         deriving (Show, Read)

exFunction = Function "examples.ex" [IntT] VoidT []


