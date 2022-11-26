module Vm
where
import Function ( Function(..), Frame(..) )
import Types ( Value(..) )

-- data Struct = Struct deriving (Show, Read)

data Vm = Vm
   { frames :: [Frame]
   , functions :: [Function] -- visible functions
   } deriving (Show, Read)
