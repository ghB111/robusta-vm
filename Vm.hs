module Vm
where
import Function (Function(..))

-- data Struct = Struct deriving (Show, Read)

type Stack = []

data VM = { stack :: Stack
          , functions :: [Function] 
          } -- pc, other stuff


-- by default everything is public
data ProgramUnit = ProgramUnit { functions :: [Function]
                            --    , structs :: [Struct]
                               }
                               deriving (Show, Read)
