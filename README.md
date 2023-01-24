# robusta-vm

Robusta-vm is a stack machine inspired by JVM written in Haskell.
Like JVM, it has dynamic linking capabilities. It has a similar frame mechanism, except that amount of variables in a frame is arbitrary. Like JVM, Robusta has heap-allocation capabilities for user code, although that heap is far more abstracted from that of JVM's.

Unlike JVM, almost everything is dynamically typed. E.g. an array can contain entities of different types. Reflection capabilities are possible through native method extension point and are not available yet.

Robusta-vm instruction set is designed to be easy to compile-to. Instructions are shared for all types. Like JVM, Robusta-vm has native functions. 'Native function' refers to a function that exists in vm runtime, but instead of being a series of instructions, it is an actual haskell function that is given the state of frame. This is how communication with world (e.g. printing to console) is implemented

## Examples

This are examples of robusta-vm programs written using an inner Haskell dsl.

```haskell
-- a regular function
ithFibFunction :: Function
ithFibFunction = Function { name = "util/fib"
                          , argTypes = [IntT]
                          , returnType = IntT
                          , instructions = instructions }
    where instructions = [ varLoad 0 -- load arg on stack
                         , dup
                         , ldc $ wrap (0 :: Int) -- base case 0
                         , ifCmpNe 5
                         , aRet -- return 0, 0th fib
                         , dup
                         , ldc $ wrap (1 :: Int) -- base case 1
                         , ifCmpNe 9
                         , aRet -- return 1, 1th fib
                         , dup
                         , const1
                         , sub
                         , invokeF "util/fib" -- get i-1
                         , varStore 1
                         , ldc $ wrap (2 :: Int)
                         , sub
                         , invokeF "util/fib" -- get i-2
                         , varLoad 1
                         , add
                         , aRet ]

-- a native function
vmPrintLn = NativeFunction "std/println" $ do
    frame@Frame{stack} <- get
    let arr = head stack
    let stringToPrint = show arr
    liftIO $ putStrLn stringToPrint
    put frame { stack = tail stack }

-- takes 0 arguements
-- returns an array that has numbers 0-3
functionThatReturnsArray :: Function
functionThatReturnsArray = Function { name = "makeArray"
                                    , argTypes = []
                                    , returnType = ArrayT
                                    , instructions = instructions }
    where instructions = [ ldcWi 4 -- 4(array size)
                         , arrNew -- arrRef()
                         , dup -- arrRef(), arrRef()
                         , ldcWi 0 -- arrRef(), arrRef(), 0
                         , ldcWi 0 -- arrRef(), arrRef(), 0
                         , arrStore -- arrRef(0)
                         , dup -- arrRef(0), arrRef(0)
                         , ldcWi 1 -- arrRef(0), arrRef(0), 1
                         , ldcWi 1 -- arrRef(0), arrRef(0), 1
                         , arrStore -- arrRef(0, 1)
                         , dup
                         , ldcWi 2
                         , ldcWi 2
                         , arrStore
                         , dup
                         , ldcWi 3
                         , ldcWi 3
                         , arrStore
                         , aRet ]

{- Expects two main integer args. Returns their sum as return code -}
sumTwoIntegers :: Function
sumTwoIntegers = Function { name = "main"
                          , argTypes = [ArrayT]
                          , returnType = IntT
                          , instructions = mainInstructions }
    where mainInstructions = [ varLoad 0 -- argsArray
                             , ldcW (0 :: Int) -- argsArray, 0
                             , arrLoad -- argsArray, arg0
                             , varStore 1 -- argsArray
                             , ldcW (1 :: Int) -- argsArray, 1
                             , arrLoad -- argsArray, arg1
                             , varLoad 1 -- argsArray, arg1, arg0
                             , invokeF "std/atoi" -- argsArray, arg1, arg0Int
                             , swap -- argsArray, arg0Int, arg1
                             , invokeF "std/atoi" -- argsArray, arg0Int, arg1Int
                             , add
                             , aRet ]


main :: IO ()
main = do
    let vm = stdVm { functions = helloWorld : stdlib }
    returnCode <- runVm vm
    putStrLn $ "Vm return code: " ++ show returnCode

```

## Try it out

This is not too amazing though

```bash
ghci ./Main.hs
main
```

## Rationale

This project's purpose is studying methods of building virtual machines and exploring Haskell pros and cons as a fp language in such goal.
