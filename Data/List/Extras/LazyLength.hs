
{-# OPTIONS_GHC -Wall -Werror #-}
-- We need this in order to ensure the rules are picked up
{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

----------------------------------------------------------------
--                                                  ~ 2008.08.17
-- |
-- Module      :  Data.List.Extras.LazyLength
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides least-strict functions for getting a list's
-- length and doing natural things with it. On GHC this module also
-- uses rewrite rules to convert certain calls to 'length' into our
-- least-strict versions.
--
-- The regular version of @length@ will traverse the entire spine
-- of the list in order to return an answer. For comparing the
-- length against some bound, that is by far too strict. Being too
-- strict can cause a space leak by expanding a lazy list before
-- necessary (or more than is ever necessary). And it can lead to
-- unnecessarily non-terminating programs when trying to determine
-- if an infinite list is longer or shorter than some finite bound.
--
-- A nicer version of @length@ would return some lazy approximation
-- of an answer which retains the proper semantics. An option for
-- doing this is to return Peano integers which can be decremented
-- as much as necessary and no further (i.e. at most one more than
-- the bound). Of course, Peano integers are woefully inefficient
-- and would wreck the cache and burn heap. This module provides
-- functions with the same lazy effect as if we used Peano integers,
-- but does so efficiently instead.
--
-- (For Peano integers see numbers:"Data.Number.Natural" or
-- non-negative:"Numeric.NonNegative.Class".)
----------------------------------------------------------------

module Data.List.Extras.LazyLength
    ( lengthBound, lengthCompare
    ) where


----------------------------------------------------------------
----------------------------------------------------------------
-- | A variant of 'length' which is least-strict for comparing
-- against a boundary length. This function is defined primarily
-- for use by rewrite rules rather than for direct use (though it's
-- fine for that too).
--
-- @lengthBound@ is polymorphic in the return of the helper
-- function so we can use 'compare' as well as '>', '>=', '==',
-- '/=', '<=', '<'. If you want to use any other functions, know
-- that we only preserve the ordering of the list's length vs the
-- boundary length and so the function should not rely on the true
-- values of either of the numbers being compared.

lengthBound      :: Int -> (Int -> Int -> a) -> [b] -> a
lengthBound n cmp xs
    | n < 0       = case xs of
                    []    -> cmp n 0
                    (_:_) -> cmp n 1
    | otherwise   = go n xs
    where
    go n' []      = cmp n' 0
    go 0  (_:_)   = cmp 0  1
    go n' (_:xs') = (go $! n'-1) xs'

{-# RULES

"lengthBound/(>)"      forall n xs. n >  length xs = lengthBound n (>)  xs
"lengthBound/(>=)"     forall n xs. n >= length xs = lengthBound n (>=) xs
"lengthBound/(==)"     forall n xs. n == length xs = lengthBound n (==) xs
"lengthBound/(/=)"     forall n xs. n /= length xs = lengthBound n (/=) xs
"lengthBound/(<=)"     forall n xs. n <= length xs = lengthBound n (<=) xs
"lengthBound/(<)"      forall n xs. n <  length xs = lengthBound n (<)  xs
"lengthBound/compare"  forall n xs.
                          compare n (length xs) = lengthBound n compare xs

"lengthBound\\(>)"     forall n xs. length xs >  n = lengthBound n (<)  xs
"lengthBound\\(>=)"    forall n xs. length xs >= n = lengthBound n (<=) xs
"lengthBound\\(==)"    forall n xs. length xs == n = lengthBound n (==) xs
"lengthBound\\(/=)"    forall n xs. length xs /= n = lengthBound n (/=) xs
"lengthBound\\(<=)"    forall n xs. length xs <= n = lengthBound n (>=) xs
"lengthBound\\(<)"     forall n xs. length xs <  n = lengthBound n (>)  xs
"lengthBound\\compare" forall n xs.
                   compare (length xs) n = lengthBound n (flip compare) xs
    #-}


----------------------------------------------------------------
----------------------------------------------------------------
-- | A variant of 'length' which is least-strict for comparing
-- the lengths of two lists. This is as strict as the length of the
-- shorter list (which allows comparing an infinite list against a
-- finite list). The function itself is trivial, but again it's
-- designed primarily for rewrite rules.
--
-- If you're going to immediately follow this with a 'zip' function
-- then see "Data.List.Extras.Pair" instead.

lengthCompare              :: [a] -> [b] -> Ordering
lengthCompare []     []     = EQ
lengthCompare (_:_)  []     = GT
lengthCompare []     (_:_)  = LT
lengthCompare (_:xs) (_:ys) = lengthCompare xs ys


{-# RULES

"lengthCompare/(>)"  forall xs ys.
                            length xs >  length ys = lengthCompare xs ys == GT
"lengthCompare/(>=)" forall xs ys.
                            length xs >= length ys = lengthCompare xs ys /= LT
"lengthCompare/(==)" forall xs ys.
                            length xs == length ys = lengthCompare xs ys == EQ
"lengthCompare/(/=)" forall xs ys.
                            length xs /= length ys = lengthCompare xs ys /= EQ
"lengthCompare/(<=)" forall xs ys.
                            length xs <= length ys = lengthCompare xs ys /= GT
"lengthCompare/(<)"  forall xs ys.
                            length xs <  length ys = lengthCompare xs ys == LT

"lengthCompare/compare"  forall n xs.
                         compare (length xs) (length ys) = lengthCompare xs ys
    #-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
