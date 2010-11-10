
-- 2008.10.12: GHC 6.10 breaks -fno-warn-orphans so that it no
-- longer suppresses the warnings for orphaned RULES. Hence -Werror
-- will make things crash on those systems, and even if that's
-- removed then -Wall will send up too many false positives which
-- may disconcert users.
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- Unfortunately GHC < 6.10 needs -fglasgow-exts in order to actually
-- parse RULES (see -ddump-rules); the -frewrite-rules flag only
-- enables the application of rules, instead of doing what we want.
-- Apparently this is fixed in 6.10.
--
-- http://hackage.haskell.org/trac/ghc/ticket/2213
-- http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg14313.html
-- OPTIONS_GHC -O2 -fglasgow-exts -frewrite-rules

----------------------------------------------------------------
--                                                  ~ 2009.04.02
-- |
-- Module      :  Data.List.Extras.LazyLength
-- Copyright   :  Copyright (c) 2007--2009 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides least-strict functions for getting a list's
-- length and doing natural things with it.
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
-- the bound). Of course, Peano integers are woefully inefficient.
-- This module provides functions with the same lazy effect but
-- does so efficiently instead.
--
-- As of version 0.3.0 the GHC rules to automatically rewrite
-- certain calls to 'length' into our least-strict versions have
-- been /removed/ for more consistent and predictable semantics.
-- All clients should explicitly call these least-strict functions
-- if they want the least-strict behavior.
----------------------------------------------------------------

module Data.List.Extras.LazyLength
    ( lengthBound, lengthCompare
    ) where


----------------------------------------------------------------
----------------------------------------------------------------
-- | A variant of 'length' which is least-strict for comparing
-- against a boundary length.
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

{- bad RULES

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
    -}


----------------------------------------------------------------
----------------------------------------------------------------
-- | A variant of 'length' which is least-strict for comparing
-- the lengths of two lists. This is as strict as the length of the
-- shorter list (which allows comparing an infinite list against a
-- finite list).
--
-- If you're going to immediately follow this with a 'zip' function
-- then see "Data.List.Extras.Pair" instead.

lengthCompare              :: [a] -> [b] -> Ordering
lengthCompare []     []     = EQ
lengthCompare (_:_)  []     = GT
lengthCompare []     (_:_)  = LT
lengthCompare (_:xs) (_:ys) = lengthCompare xs ys


{- bad RULES

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

"lengthCompare/compare"  forall xs ys.
                         compare (length xs) (length ys) = lengthCompare xs ys
    -}

----------------------------------------------------------------
----------------------------------------------------------- fin.
