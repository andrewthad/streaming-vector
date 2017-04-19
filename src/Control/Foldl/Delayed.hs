{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Foldl.Delayed 
  ( DelayedFold(..)
  , DelayedFoldM(..)
  , mean
  , fold
  , foldM
  , scanM
  ) where

import qualified Data.Foldable as F
import Control.Monad hiding (foldM)

-- | The integer represents the number of values that must be
--   received before the fold can output a value. This must always
--   be non-negative. If it is not, then the Applicative instance
--   is not law-abiding.
data DelayedFold a b 
  = forall x. DelayedFold {-# UNPACK #-} !Int (x -> a -> x) x (x -> b)

data DelayedFoldM m a b
  = forall x. DelayedFoldM {-# UNPACK #-} !Int (x -> a -> m x) (m x) (x -> m b)

data Pair a b = Pair !a !b
data WithInt a = WithInt {-# UNPACK #-} !Int !a

deriving instance Functor (DelayedFold a)

instance Applicative (DelayedFold a) where
  pure b    = DelayedFold 0 (\() _ -> ()) () (\() -> b)
  {-# INLINE pure #-}

  (DelayedFold reqL stepL beginL doneL) <*> (DelayedFold reqR stepR beginR doneR) =
      let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
          begin = Pair beginL beginR
          done (Pair xL xR) = doneL xL (doneR xR)
          req = max reqL reqR
       in DelayedFold req step begin done
  {-# INLINE (<*>) #-}

mean :: Fractional a => DelayedFold a a
mean = DelayedFold 1 step begin done
  where
  begin = Pair 0 0
  step (Pair x n) y = Pair ((x * n + y) / (n + 1)) (n + 1)
  done (Pair x _) = x
  
fold :: Foldable f => DelayedFold a b -> f a -> Maybe b
fold (DelayedFold req step begin done) as = case F.foldl' cons (WithInt 0 begin) as of
  WithInt n r -> if n < req
    then Nothing
    else Just (done r)
  where
  cons (WithInt n x) a = WithInt (n + 1) (step x a)

foldM :: (Foldable f, Monad m) => DelayedFoldM m a b -> f a -> m (Maybe b)
foldM (DelayedFoldM req step begin done) as = do
  x0 <- begin
  WithInt n r <- F.foldlM cons (WithInt 0 x0) as
  -- This could be improved by not keeping track of the
  -- counter once it exceeds the threshold.
  if n < req
    then return Nothing
    else liftM Just (done r)
  where
  cons (WithInt n x) a = liftM (WithInt (n + 1)) (step x a)

scanM :: forall m a b. Monad m => DelayedFoldM m a b -> [a] -> m [b]
scanM (DelayedFoldM req step begin done) as = do
  x0 <- begin
  go1 done step (WithInt 0 x0) as
  where
  go1 :: forall x. (x -> m b) -> (x -> a -> m x) -> WithInt x -> [a] -> m [b]
  go1 done' step' (WithInt n x) as' = if n < req - 1
    then case as' of
      [] -> return []
      a : as -> do
        x' <- step' x a
        go1 done' step' (WithInt (n + 1) x') as
    else go2 done' step' x as'
  go2 :: forall x. (x -> m b) -> (x -> a -> m x) -> x -> [a] -> m [b]
  go2 done' step' x as' = 
    case as' of
      [] -> return []
      a : as -> do
        x' <- step' x a
        b <- done' x'
        bs <- go2 done' step' x' as
        return (b : bs)



