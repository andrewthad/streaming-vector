{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.DocTest
import Streaming.Vector
import Control.Monad.ST
import qualified Data.List as L
import qualified Streaming.Prelude as SMP
import qualified Control.Foldl.Delayed as CFD
import qualified Control.Foldl.Delayed.Window as CFDW

prop_slidingBoundsFst :: Int -> [Word] -> Property
prop_slidingBoundsFst windowSize xs = 
  (length xs - windowSize >= 0 && windowSize > 0) 
  ==>
  ((==)
     (map fst (runST (SMP.toList_ (slidingBounds windowSize (SMP.each xs)))))
     (dropEnd windowSize xs)
  )

prop_delay' :: Int -> [Word] -> Property
prop_delay' windowSize xs = 
  (length xs - windowSize >= 0 && windowSize > 0) 
  ==>
  ((==)
     (runST (CFD.scanM (CFDW.delay' windowSize) xs))
     (dropEnd windowSize xs)
  )

prop_last' :: [Word] -> Bool
prop_last' xs = xs == runST (CFD.scanM CFDW.last' xs)

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = L.reverse (drop n (L.reverse xs))

return []
runTests = $quickCheckAll

main :: IO ()
main = do
  runTests
  doctest ["-isrc", "src/Streaming/Vector.hs"]
  

