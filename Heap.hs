{-# LANGUAGE NamedFieldPuns #-}

module Heap ( Heap
            , HeapKey
            , makeEmptyHeap
            , alloc
            , delete
            , get
            , put )
where

import Data.List ( find )
import Data.Maybe ( fromJust
                  , isNothing )

import Types ( Value(..) )

type HeapKey = Integer

{-
   Heap is a mapping of ids (think memory address) to actual values
-}
newtype Heap = Heap { mapping  :: [(HeapKey, [Value])] }
   deriving (Show, Read)

nextId :: Heap -> HeapKey
nextId (Heap {mapping=[]}) = 1
nextId (Heap {mapping}) = 1 + (maximum . map fst) mapping

makeEmptyHeap :: Heap
makeEmptyHeap = Heap []

nullPtr :: HeapKey
nullPtr = 0

alloc :: Heap -> Int -> (HeapKey, Heap)
alloc heap@Heap{mapping} size = (key, newHeap)
   where newHeap = heap { mapping = (key, replicate size (VoidV ())) : mapping }
         key = nextId heap

delete :: Heap -> HeapKey -> Heap
delete Heap{mapping} key = Heap $ filter (\x -> fst x /= key) mapping

get :: Heap -> HeapKey -> [Value]
get Heap{mapping} key = foundArray
   where maybeFoundPair = find (\x -> fst x == key) mapping
         foundPair | isNothing maybeFoundPair = error "Unknown key on heap"
                   | otherwise                = fromJust maybeFoundPair
         foundArray = snd foundPair

put :: Heap -> HeapKey -> [Value] -> Heap
put heap@Heap{mapping} key newValue = 
   if length oldValue /= length newValue
      then error "Cannot change length of array"
      else newHeap
   where newHeap = Heap $ (key, newValue) : filter (\x -> fst x /= key) mapping
         oldValue = get heap key
