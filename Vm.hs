module Vm
where
import Function ( Function(..), Frame(..) )
import Types ( Value(..) )
import Heap ( Heap(..) )

data Vm = Vm
   { frames :: [Frame]
   , functions :: [Function] -- visible functions
   , heap :: Heap
   } deriving (Show, Read)
