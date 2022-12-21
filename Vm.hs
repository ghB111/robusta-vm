module Vm
where
import Function ( Function(..), Frame(..) )
import Types ( Value(..) )

-- data Struct = Struct deriving (Show, Read)

type Heap = [Value]

data Vm = Vm
   { frames :: [Frame]
   , functions :: [Function] -- visible functions
   , heap :: Heap -- the bogus heap
   } deriving (Show, Read)
