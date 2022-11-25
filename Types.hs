{-# LANGUAGE FlexibleInstances #-}
module Types ( Value(..), Type(..), vType, wrap, makeDefault )
where

data Value 
    = VoidV ()
    | IntV Int
    | StringV String
    deriving (Show, Read)

data Type
    = VoidT
    | IntT
    | StringT
    deriving (Show, Read)

vType :: Value -> Type
vType (VoidV _) = VoidT
vType (IntV _) = IntT
vType (StringV _) = StringT

-- here is our billion dollar mistake
makeDefault :: Type -> Value
makeDefault VoidT   = VoidV ()
makeDefault IntT    = wrap (0 :: Int)
makeDefault StringT = wrap ""

class VmValue a where
    wrap :: a -> Value

instance VmValue Int where
    wrap x = IntV x

instance VmValue ([] Char) where
    wrap x = StringV x

instance VmValue () where
    wrap x = VoidV x
