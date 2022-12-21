{-# LANGUAGE FlexibleInstances #-}

module Types ( Value(..), Type(..), vType, wrap, VmValue, makeDefault )
where

type HeapRef = Int

data Value 
    = VoidV ()
    | IntV Int
    | CharV Char
    | ArrayV HeapRef
    deriving (Show, Read)

data Type
    = VoidT
    | IntT
    | CharT
    | ArrayT
    deriving (Show, Read)

vType :: Value -> Type
vType (VoidV _) = VoidT
vType (IntV _) = IntT
vType (CharV _) = CharT
vType (ArrayV _) = ArrayT

-- here is our billion dollar mistake
makeDefault :: Type -> Value
makeDefault VoidT   = VoidV ()
makeDefault IntT    = wrap (0 :: Int)
makeDefault ArrayT  = wrap (0 :: Int) -- todo this is a funny wtf moment

class VmValue a where
    wrap :: a -> Value

instance VmValue Int where
    wrap x = IntV x

instance VmValue Char where
    wrap x = CharV x

instance VmValue () where
    wrap x = VoidV x
