# robusta-vm

Robusta-vm is a stack machine inspired by JVM written in Haskell.
Like JVM, it has dynamic linking capabilities, functions. It has a similar frame model, except that amount of variables in a frame is arbitrary. Heap-allocated objects are planned.

Unlike JVM, almost everything is dynamically typed. E.g. an array can contain entities of different types, although strictness could be deduced by compiler. Reflection capabilities are planned, since they can help building a dynamicly-typed language.

Robusta-vm instruction set is designed to be easy to compiled-to. Instructions are shared for all types. Like JVM, Robusta-vm has native functions. At the time of writing, 'native function' refers to a function that exists in vm runtime, but instead of being a series of instructions, it is an actual haskell function that is given the state of frame. This is how communication with world (e.g. printing to console) is implemented

```haskell
-- a regular function
ithFibFunction :: Function
ithFibFunction = Function { name = "util/fib"
                          , argTypes = [IntT]
                          , returnType = IntT
                          , instructions = instructions }
    where instructions = [ iLoad 0 -- load arg on stack
                         , dup
                         , ldc $ wrap (0 :: Int) -- base case 0
                         , ifICmpNe 5
                         , iRet -- return 0, 0th fib
                         , dup
                         , ldc $ wrap (1 :: Int) -- base case 1
                         , ifICmpNe 9
                         , iRet -- return 1, 1th fib
                         , dup
                         , iConst1
                         , iSub
                         , invokeF "util/fib" -- get i-1
                         , iStore 1
                         , ldc $ wrap (2 :: Int)
                         , iSub
                         , invokeF "util/fib" -- get i-2
                         , iLoad 1
                         , iAdd
                         , iRet ]

-- a native function
vmPrintLn = NativeFunction "std/println" $ do
    frame@Frame{stack} <- get
    let arr = head stack
    let stringToPrint = toString arr
    liftIO $ putStrLn stringToPrint
    put frame { stack = tail stack }
```

Apart from native functions, all state changes are pure.

## Rationale

This project's purpose is studying virtual machines and exploring Haskell pros and cons as a fp language in such conditions
