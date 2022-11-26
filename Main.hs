import Vm 
import Function
import Types
import Instruction
import Execution

baseVm :: Vm
baseVm = Vm { frames = [baseFrame], functions = [] }

baseFrame :: Frame
baseFrame = Frame { variables = [], pc = 0, stack = [], function = baseFunction }

baseFunction :: Function
baseFunction = Function { name = "bootstrap"
                        , argTypes = [StringT]
                        , returnType = IntT
                        , instructions = baseFunctionInstructions }

baseFunctionInstructions :: [Instruction]
baseFunctionInstructions = [ (SpecialInstructionC $ InvokeF "main")
                           , (SpecialInstructionC IReturn) ]

mainFuncExample :: Function
mainFuncExample = Function { name = "main"
                           , argTypes = []
                           , returnType = IntT
                           , instructions = instructions }
    where instructions = [ (FrameInstructionC $ Ldc $ wrap (3 :: Int))
                         , (FrameInstructionC $ Goto 5)
                         , (FrameInstructionC $ Ldc $ wrap (1 :: Int))
                         , (FrameInstructionC $ Ldc $ wrap (5 :: Int))
                         , (FrameInstructionC $ Ldc $ wrap (10 :: Int))
                         , (FrameInstructionC $ INeg)
                         , (SpecialInstructionC $ IReturn) ]

exVm :: Vm
exVm = baseVm { functions = [mainFuncExample] }

main = do
    print $ runVm exVm
