{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad(liftM, liftM2, when)
import Control.Monad.ST(runST)
import Data.List(inits, tails)
import BinSearch(expSearch)
import qualified MFifoQ
import Test.QuickCheck(quickCheck, (==>), Property)

runningMax :: (Ord a) => Int -> [a] -> [a]
runningMax n stream = runST (MFifoQ.new' n >>= runningMax' stream)
  where
    runningMax' xs fifo =
        liftM2 (++)
        (handleBeginning fifo beginning)
        (handleRest      fifo xs middle)
      where
        (beginning, middle) = splitAt n xs

    handleBeginning fifo = mapM (\x -> appendToQueue fifo x >> getMax fifo)

    -- given leaving/entering window
    handleRest fifo ls     []     = handleEnd fifo ls
    handleRest fifo (l:ls) (x:xs) = do
      rmax <- getMax fifo
      when (l == rmax) $ popMax fifo
      appendToQueue fifo x
      rmax' <- getMax fifo
      rest <- handleRest fifo ls xs
      return (rmax':rest)
    handleRest _    []     _      = error "Leaving list should be longer than entering list..."

    handleEnd _    [] = return []
    handleEnd _    [_] = return [] -- we show max AFTER removal of leaver
    handleEnd fifo (l:ls) = do
      rmax <- getMax fifo
      when (l == rmax) $ popMax fifo
      rmax' <- getMax fifo
      rest <- handleEnd fifo ls
      return (rmax':rest)

    appendToQueue fifo x = do
      l <- MFifoQ.length fifo
      insertionPoint <- expSearchFromEnd l
      case insertionPoint of
        Nothing -> do
            MFifoQ.truncate fifo 0
            MFifoQ.append fifo (1 :: Int, x)
        Just i  -> do
          (count, val) <- MFifoQ.readAt fifo i
          if val == x
            then
              MFifoQ.writeAt fifo i (count+1, val)
            else do
              MFifoQ.truncate fifo (i+1)
              MFifoQ.append fifo (1, x)
      where
        expSearchFromEnd l =
            (liftM . fmap) (rIndex l) $
            expSearch (liftM ((x <) . snd) . MFifoQ.readAt fifo . rIndex l)
                      0 (l - 1)
        rIndex l = (l - 1 -)

    getMax fifo = do
      (_, rmax) <- MFifoQ.readAt fifo 0
      return rmax
    popMax fifo = do
      (count, rmax) <- MFifoQ.readAt fifo 0
      if count == 1
        then
          MFifoQ.popFirst fifo
        else
          MFifoQ.writeAt fifo 0 (count-1, rmax)

naiveImpl :: (Ord a) => Int -> [a] -> [a]
naiveImpl n xs =
    map (maximum . take n) . takeWhile (not . null) $ start ++ end
  where
    start = tail $ inits (take n xs)
    end   = tail $ tails xs

prop_same :: Int -> [Int] -> Property
prop_same n ls = (n > 0) ==> naiveImpl n ls == runningMax n ls

main :: IO ()
main = do
  print =<< expSearch (return . (>=100000)) 0 (100000::Int)
  let l :: [Int]
      l = [3,2,1,4,6,10,5,20,1]

  quickCheck prop_same

  print $ naiveImpl 3 l
  print $ runningMax 3 l

  print $ naiveImpl 3 ([0]::[Int])
  print $ runningMax 3 ([0]::[Int])
