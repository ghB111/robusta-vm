module Dsl ( iRet
           , ret
           , invokeF
           , dup
           , pop
           , swap
           , nop
           , iAdd
           , iSub
           , iMul
           , iDiv
           , iNeg
           , iConst0
           , iConst1
           , ldc
           , ldcW
           , goto
           , iLoad
           , iStore
           , ifICmpNe
           , arrLen
           , newArr
           , arrLoad
           , arrStore 
           -- subject to move to macros
           , ldcString )
where

import Vm 
import Function
import Types
import Instruction
import Execution



iRet    = SpecialInstructionC $ IReturn
ret     = SpecialInstructionC $ Return
invokeF = SpecialInstructionC . InvokeF

-- todo use macros
dup      = FrameInstructionC $ Dup
pop      = FrameInstructionC $ Pop
swap     = FrameInstructionC $ Swap
nop      = FrameInstructionC $ Nop
iAdd     = FrameInstructionC $ IAdd
iSub     = FrameInstructionC $ ISub
iMul     = FrameInstructionC $ IMul
iDiv     = FrameInstructionC $ IDiv
iNeg     = FrameInstructionC $ INeg
iConst0  = FrameInstructionC $ IConst0
iConst1  = FrameInstructionC $ IConst1
ldc      = FrameInstructionC . Ldc
goto     = FrameInstructionC . Goto
iLoad    = FrameInstructionC . ILoad
iStore   = FrameInstructionC . IStore
ifICmpNe = FrameInstructionC . IfICmpNe
arrLen   = HeapInstructionC $ ArrLen
newArr   = HeapInstructionC $ NewArr
arrLoad  = HeapInstructionC $ ArrLoad
arrStore = HeapInstructionC $ ArrStore

ldcW :: VmValue a => a -> Instruction
ldcW = ldc . wrap

-- this will move to macros later

{- Makes an array on stack, loads all chars to it. stack: () -> stringArrayRef -}
ldcString :: String -> [Instruction]
ldcString str = [ ldcW $ length str, newArr ] ++ loadStrInstr
    where strIndexed :: [(Int, Char)]
          strIndexed = zip [0..] str
          loadStrInstr :: [Instruction]
          loadStrInstr = concat [ [dup, ldcW idx, ldcW ch, arrStore] | (idx, ch) <- strIndexed ]
