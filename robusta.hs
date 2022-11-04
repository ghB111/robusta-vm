
type Stack = []

type Instruction = Instruction
type Type = StringT | IntT | ObjRef

data Function = Function { name :: String
                         , argTypes :: [Type]
                         , instructions :: [Instruction] } deriving (Show, Read)
data Struct = Struct deriving (Show, Read)

-- by default everything is public
data ProgramUnit = ProgramUnit { functions :: [Function]
                               , structs :: [Struct] } deriving (Show, Read)
