{-# LANGUAGE FlexibleInstances #-}
module Types ( Value(..), Type(..) )
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

class VmValue a where
    wrap :: a -> Value

instance VmValue Int where
    wrap x = IntV x

instance VmValue ([] Char) where
    wrap x = StringV x

instance VmValue () where
    wrap x = VoidV x

-- -- class VmTypeConvertible a
-- --     toVmType :: a -> Type

