module Types ( Type (..) )
where

-- things that can be on stack
data Type = VoidT | StringT | IntT -- | ObjRefT

class VmValue a
    getType

class VmTypeConvertible a
    toVmType :: a -> Type

