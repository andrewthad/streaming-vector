{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-| The functions in this module work with streams in one of
    two ways. They either convert between streams and vectors
    or they transform streams using mutable vectors under the
    hood to improve the performance of the transformation.
    Functions in this module whose name begins with @sliding@
    performing a calculation of a window of the most recent
    values in the stream.
-}

module Streaming.Vector 
  ( slidingBounds
  ) where

import Streaming.Internal (Stream(Return,Effect,Step))
import Streaming.Prelude (Of((:>)))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad
import qualified Data.Primitive.Array as PA
import qualified Streaming.Prelude as SMP
import qualified Streaming.Prelude as S

-- | Outputs the current value paired with an older value that
--   is a specified distance away. This waits for the specified 
--   number of values to pass through before it begins to output
--   anything.
--
--   >>> S.print $ slidingBounds 5 $ S.each ['a'..'h']
--   ('a','f')
--   ('b','g')
--   ('c','h')
--
--   This function can be used to satisfy many of the use cases that 
--   'SMP.slidingWindow' satisfies, but it is much more efficient.
slidingBounds :: forall a m b. PrimMonad m 
  => Int -- ^ Window size, must be positive
  -> Stream (Of a) m b -- ^ Values
  -> Stream (Of (a,a)) m b
slidingBounds !n' stream = Effect $ do
  let !n = max n' 1
  marr <- PA.newArray n slidingBoundsError
  let go1 :: Int -> Stream (Of a) m b -> Stream (Of (a,a)) m b
      go1 !ix s1 = case s1 of
        Return r -> Return r
        Effect m -> Effect (liftM (go1 ix) m)
        Step (a :> s2) -> if ix < n - 1
          then Effect (PA.writeArray marr ix a >> return (go1 (ix + 1) s2))
          else Effect (PA.writeArray marr ix a >> return (go2 0 s2))
      -- go2 has a precondition that ix is less than the upper bound
      go2 :: Int -> Stream (Of a) m b -> Stream (Of (a,a)) m b
      go2 !ix s1 = case s1 of
        Return r -> Return r
        Effect m -> Effect (liftM (go2 ix) m)
        Step (new :> s2) -> Effect $ do
          !old <- PA.readArray marr ix
          PA.writeArray marr ix new
          return (Step ((old,new) :> go2 (rem (ix + 1) n) s2))
  return (go1 0 stream)

slidingBoundsError :: a
slidingBoundsError = error "slidingBounds: bad array access, implementation mistake."
{-# NOINLINE slidingBoundsError #-}

--
-- data DelayedScan a b = forall x. DelayedScan (x -> a -> Either (Fold a b) x) x 
-- data Scan = forall x. Scan (x -> a -> x) 
-- 
-- 
-- -- | Discards the original value
-- slidingMinimum_ :: forall a m b. PrimMonad m 
--   => Int -- ^ Window size, must be positive
--   -> Stream (Of a) m b -- ^ Values
--   -> Stream (Of a) m b
-- slidingMinimum_ = error "write me"
-- 
-- data DequeStatic s = DequeStatic 
--   { dequeStaticArray :: {-# UNPACK #-} !(MutableByteArray s)
--   , dequeStaticSize :: {-# UNPACK #-} !Int
--   }
-- 
-- data DequeDynamic = DequeDynamic
--   { dequeDynamicStart :: {-# UNPACK #-} !Int
--   , dequeDynamicLength :: {-# UNPACK #-} !Int
--   }
-- 
-- newDeque :: Int -> m (DequeStatic (PrimState m), DequeDynamic)
-- newDeque size = do
--   let arrLen = toArrayLength size
--   marr <- PA.newByteArray (arrLen * sizeOf (undefined :: Int))
--   return (DequeStatic marr size, DequeDynamic (twice size))
-- 
-- toArrayLength :: Int -> Int
-- toArrayLength x = unsafeShiftL x 2
-- 
-- consDeque :: DequeStatic (PrimState m) -> DequeDynamic -> Int -> m ()
-- consDeque 
-- 
-- twice :: Int -> Int
-- twice x = unsafeShiftL x 1


