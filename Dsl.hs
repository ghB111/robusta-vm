module Dsl ( aRet
           , ret
           , invokeF
           , dup
           , pop
           , swap
           , nop
           , add
           , sub
           , mul
           , div
           , neg
           , const0
           , const1
           , ldc
           , ldcW
           , ldcWi
           , goto
           , varLoad
           , varStore
           , ifCmpNe
           , arrLen
           , arrNew
           , arrLoad
           , arrStore 
           -- subject to move to macros
           , ldcString )
where

import Prelude hiding (div)

import Vm 
import Function
import Types
import Instruction
import Execution



aRet    = SpecialInstructionC $ AReturn
ret     = SpecialInstructionC $ Return
invokeF = SpecialInstructionC . InvokeF

-- todo use macros
dup      = FrameInstructionC $ Dup
pop      = FrameInstructionC $ Pop
swap     = FrameInstructionC $ Swap
nop      = FrameInstructionC $ Nop
add     = FrameInstructionC $ Add
sub     = FrameInstructionC $ Sub
mul     = FrameInstructionC $ Mul
div     = FrameInstructionC $ Div
neg     = FrameInstructionC $ Neg
const0  = FrameInstructionC $ Const0
const1  = FrameInstructionC $ Const1
ldc      = FrameInstructionC . Ldc
goto     = FrameInstructionC . Goto
varLoad    = FrameInstructionC . VarLoad
varStore   = FrameInstructionC . VarStore
ifCmpNe = FrameInstructionC . IfCmpNe
arrLen   = HeapInstructionC $ ArrLen
arrNew   = HeapInstructionC $ ArrNew
arrLoad  = HeapInstructionC $ ArrLoad
arrStore = HeapInstructionC $ ArrStore

ldcW :: VmValue a => a -> Instruction
ldcW = ldc . wrap

ldcWi :: Int -> Instruction
ldcWi = ldcW

-- this will move to macros later

{- Makes an array on stack, loads all chars to it. stack: () -> stringArrayRef -}
ldcString :: String -> [Instruction]
ldcString str = [ ldcW $ length str, arrNew ] ++ loadStrInstr
    where strIndexed :: [(Int, Char)]
          strIndexed = zip [0..] str
          loadStrInstr :: [Instruction]
          loadStrInstr = concat [ [dup, ldcW idx, ldcW ch, arrStore] | (idx, ch) <- strIndexed ]
