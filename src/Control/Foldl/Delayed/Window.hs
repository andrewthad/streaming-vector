{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Foldl.Delayed.Window
  ( delay'
  , last'
  ) where

import Control.Foldl.Delayed (DelayedFoldM(..))
import Control.Monad.Primitive (PrimMonad,PrimState)
import qualified Data.Primitive.Array as PA

-- shutters' :: forall m a. PrimMonad m => Int -> DelayedFoldM m a (a,a)
-- shutters' !n' = DelayedFoldM n step begin done
--   where
--   n :: Int
--   !n = max n' 1
--   begin :: m (IndexMutableArray a)
--   begin = do
--     marr <- PA.newArray (n + 1) shuttersError
--     return (IndexMutableArray 0 marr shuttersError)
--   step :: IndexMutableArray a -> a -> m (IndexMutableArray a)
--   step (IndexMutableArray i marr a) =
--   done :: IndexMutableArray a -> m (a,a)
--   done (IndexMutableArray i marr a) = 

-- | Provide an older value
delay' :: forall m a. PrimMonad m => Int -> DelayedFoldM m a a
delay' !n' = DelayedFoldM n step begin done
  where
  n :: Int
  !n = max (n' + 1) 2
  begin :: m (IndexMutableArray (PrimState m) a)
  begin = do
    marr <- PA.newArray (n + 1) shuttersError
    return (IndexMutableArray 0 marr)
  step :: IndexMutableArray (PrimState m) a -> a -> m (IndexMutableArray (PrimState m) a)
  step (IndexMutableArray i marr) a = do
    PA.writeArray marr i a
    return (IndexMutableArray (nextIndex i) marr)
  done :: IndexMutableArray (PrimState m) a -> m a
  done (IndexMutableArray i marr) = do
    PA.readArray marr i
  nextIndex :: Int -> Int
  nextIndex i = if i < n - 1
    then i + 1
    else 0
  prevIndex :: Int -> Int
  prevIndex i = if i > 0
    then i - 1
    else n - 1

last' :: Monad m => DelayedFoldM m a a
last' = DelayedFoldM 1 step begin done
  where
  begin = return (Boxed lastError)
  step _ a = return (Boxed a)
  done (Boxed a) = return a

data IndexMutableArray s a = IndexMutableArray
  {-# UNPACK #-} !Int {-# UNPACK #-} !(PA.MutableArray s a)

data Boxed a = Boxed a

shuttersError :: a
shuttersError = error "shutters': bad array access, implementation mistake."
{-# NOINLINE shuttersError #-}

lastError :: a
lastError = error "last: bad element access, implementation mistake."
{-# NOINLINE lastError #-}

