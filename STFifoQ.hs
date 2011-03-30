module STFifoQ(
    STFifoQ,
    new, readAt, writeAt, append,
    popFirst, truncate, elems, length)
where

import Prelude hiding (length, truncate)
import Control.Monad.ST(ST, runST)
import Control.Monad(liftM2, join)
import Data.Array.ST(STArray)
import Data.Array.MArray(MArray, newArray_, readArray, writeArray, getBounds)
import Data.STRef(STRef, newSTRef, readSTRef, writeSTRef)

data STFifoQ s a = STFifoQ {
     sfqArray :: STArray s Int a,
     sfqHead  :: STRef s Int,
     sfqCount :: STRef s Int
    }

new :: Int -> ST s (STFifoQ s a)
new i = do
  arr <- newArray_ (0, i-1)
  head <- newSTRef 0
  count <- newSTRef 0
  return $ STFifoQ arr head count

-- Only read/mutate array
inFQ_ :: STFifoQ s a -> (STArray s Int a -> Int -> (Int, Int) -> ST s b) -> ST s b
inFQ_ fifo func = do
  (0, high) <- getBounds arr
  join $ liftM2 (curry (func arr (high+1))) (readSTRef (sfqHead fifo)) (readSTRef (sfqCount fifo))
  where
    arr = sfqArray fifo

-- Read/mutate array AND the position indices
inFQ :: STFifoQ s a -> (STArray s Int a -> Int -> (Int, Int) -> ST s ((Int, Int), b)) -> ST s b
inFQ fifo func = do
  ((hd', cnt'), result) <- inFQ_ fifo func
  writeSTRef (sfqHead fifo) hd'
  writeSTRef (sfqCount fifo) cnt'
  return result

readAt :: STFifoQ s a -> Int -> ST s a
readAt fifo i = inFQ_ fifo doRead
  where
    doRead arr size (hd, cnt)
      | i >= cnt   =  fail "Invalid index"
      | otherwise  =  readArray arr ((hd + i) `mod` size)

writeAt :: STFifoQ s a -> Int -> a -> ST s ()
writeAt fifo i x = inFQ_ fifo doWrite
  where
    doWrite arr size (hd, cnt)
      | i >= cnt   =  fail "Invalid index"
      | otherwise  =  writeArray arr ((hd + i) `mod` size) x

append :: STFifoQ s a -> a -> ST s ()
append fifo x = inFQ fifo doWrite
  where
    doWrite arr size (hd, cnt)
      | cnt >= size  =  fail "Append too much"
      | otherwise    =  writeArray arr idx x >>
                        return ((hd, cnt+1), ())
      where
        idx = (hd + cnt) `mod` size

popFirst :: STFifoQ s a -> ST s ()
popFirst fifo = inFQ fifo doPop
  where
    doPop arr size (hd, cnt)
      | size <= 0  =  fail "Pop empty queue"
      | otherwise  =  return ((hd+1, cnt-1), ())

truncate :: STFifoQ s a -> Int -> ST s ()
truncate fifo l = inFQ fifo doTruncate
  where
    doTruncate arr size (hd, cnt) = return ((hd, min l cnt), ())

elems :: STFifoQ s a -> ST s [a]
elems fifo = inFQ_ fifo getElems
  where
    getElems arr size (hd, cnt) = mapM (readArray arr) (a ++ b)
      where
        a = [hd..min (hd+cnt) size - 1]
        b | hd+cnt-1 >= size = [0..hd+cnt-1 - size]
          | otherwise      = []

length :: STFifoQ s a -> ST s Int
length fifo = inFQ_ fifo $ \arr size (hd, cnt) -> return cnt
