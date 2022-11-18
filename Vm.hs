module Vm
where
import Function ( Function(..), Frame(..) )
import Types ( Value(..) )

-- data Struct = Struct deriving (Show, Read)

{-
   Vm has one common stack that is shared between
   function calls. I.e. if `foo` calls `bar`, `bar`
   can access `foo`s stack. This is not done on purpose
-}

data Vm = Vm
   { frames :: [Frame]
   , functions :: [Function] 
   } deriving (Show, Read)

