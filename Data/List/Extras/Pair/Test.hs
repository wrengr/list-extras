{---------------------------------------------------------------
-- wren ng thornton <wren@cpan.org>                 ~ 2008.07.09
---------------------------------------------------------------}

{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module provides a test suite for Data.List.Extras.Pair
module Data.List.Extras.Pair.Test
    ( naivePair
    , timeIO, main
    , module Data.List.Extras.Pair
    ) where

import Data.List.Extras.Pair
import System.CPUTime (getCPUTime)

import qualified Test.HUnit      as HU
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
----------------------------------------------------------------


naivePair :: [a] -> [b] -> Maybe [(a,b)]
naivePair xs ys
    | length xs == length ys = Just (zip xs ys)
    | otherwise              = Nothing

prop_naivePair :: ( Eq a, Show a, QC.Arbitrary a
                  , Eq b, Show b, QC.Arbitrary b
                  ) => [a] -> [b] -> Bool
prop_naivePair xs ys =
    pair xs ys == naivePair xs ys


prop_WithVsBy :: ( Eq a, Show a, QC.Arbitrary a
                 , Eq b, Show b, QC.Arbitrary b
                 ) => [a] -> [b] -> Bool
prop_WithVsBy xs ys =
    pairWith (,) xs ys == pairBy (:) [] xs ys


-- TODO: we need a better instance of Arbitrary for lists to make them longer than our smallcheck depth.

-- Convert from picoseconds to sec
getCPUTimeInSeconds :: IO Double
getCPUTimeInSeconds  = do t <- getCPUTime; return (fromInteger t * 1e-12)


-- Helpful for our programs. From within GHCi just do :set +s
timeIO   :: IO a -> IO a
timeIO mx = do t1 <- getCPUTimeInSeconds
               x  <- mx
               t2 <- getCPUTimeInSeconds
               putStrLn $! "time in sec: " ++ show (t2 - t1)
               return x

main :: IO ()
main  = do
    putStrLn ""
    putStrLn (replicate 80 '~')
    
    putStrLn "smallcheck{ type = [()]; depth = 50 }"
    SC.smallCheck 50 (prop_naivePair :: [()] -> [()] -> Bool)
    SC.smallCheck 50 (prop_WithVsBy  :: [()] -> [()] -> Bool)
    putStrLn ""
    
    putStrLn "quickcheck{ type = [Int]; maxTest = 10000 }"
    QC.check (QC.defaultConfig { QC.configMaxTest = 10000 })
             (prop_naivePair :: [Int] -> [Int] -> Bool)
    QC.check (QC.defaultConfig { QC.configMaxTest = 10000 })
             (prop_WithVsBy  :: [Int] -> [Int] -> Bool)
    putStrLn ""
    
    putStrLn "hunit{ millionEnum }"
    HU.runTestTT (HU.TestList
        [ HU.TestCase $ HU.assertBool ""
        $ prop_naivePair millionEnum millionEnum
        ])
    putStrLn ""
    
    where
    millionEnum :: [Int]
    millionEnum  = [1..1000000]

data E = forall e. (QC.Testable e, SC.Testable e) => E e

----------------------------------------------------------------
----------------------------------------------------------- fin.
