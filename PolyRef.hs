{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeFamilies #-}
module PolyRef(PolyRef(..)) where

import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Control.Monad.ST(ST)
import Data.STRef(STRef, newSTRef, readSTRef, writeSTRef)

class PolyRef r where
  type RefMonad r :: * -> *
  newPolyRef :: a -> RefMonad r (r a)
  writePolyRef :: r a -> a -> RefMonad r ()
  readPolyRef :: r a -> RefMonad r a

{-- IO --}

instance PolyRef IORef where
  type RefMonad IORef = IO
  newPolyRef = newIORef
  writePolyRef = writeIORef
  readPolyRef = readIORef

{-- ST --}

instance PolyRef (STRef s) where
  type RefMonad (STRef s) = ST s
  newPolyRef = newSTRef
  writePolyRef = writeSTRef
  readPolyRef = readSTRef
