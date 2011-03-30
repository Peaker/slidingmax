module BinSearch(searchFromTo, expSearch) where

import Control.Monad(liftM)

searchFromTo :: (Monad m, Integral a) => (a -> m Bool) -> a -> a -> m (Maybe a)
searchFromTo mp l h
  | l > h     = return Nothing
  | otherwise = do
    p <- mp h
    if p then Just `liftM` searchSafeRange mp l h
         else return Nothing

-- | Like 'searchFromTo', but assuming @l <= h && p h@.
searchSafeRange :: (Monad m, Integral a) => (a -> m Bool) -> a -> a -> m a
searchSafeRange mp l h
  | l == h = return l
  | otherwise = do
    p <- mp m
    if p then searchSafeRange mp l m
         else searchSafeRange mp (m+1) h
  -- Stay within @min 0 l .. max 0 h@ to avoid overflow.
  where
    m = l `div` 2 + h `div` 2	-- If l < h, then l <= m < h

expSearch :: (Monad m, Integral a) => (a -> m Bool) -> a -> a -> m (Maybe a)
expSearch trophyLower low high = go 0 0
  where
    go lastBefore current
      | pos > high = searchFromTo trophyLower lastBefore high
      | otherwise      = do
        p <- trophyLower pos
        if p then searchFromTo trophyLower lastBefore pos
             else go pos (current*2+1)
      where
        pos = low+current
