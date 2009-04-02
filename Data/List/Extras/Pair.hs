
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.04.02
-- |
-- Module      :  Data.List.Extras.Pair
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides safe zipping functions which will fail
-- (return 'Nothing') on uneven length lists.
----------------------------------------------------------------

module Data.List.Extras.Pair
    (
    -- * Safe functions for zipping lists
      pairWithBy, pairWith, pairBy, pair
    
    -- * Special safe zipping functions
    , biject, biject'
    
    -- * New (unsafe) zipping functions
    , zipWithBy, zipBy
    ) where


----------------------------------------------------------------
----------------------------------------------------------------
-- TODO: benchmark fusion performance of:
--
--     foldr cons nil ... zipWith (,)
--     zipWithBy (,) cons nil
--
-- ...That is, the latter is a manual fusion of the former, but
-- does zip/zipWith have a special ability to fuse with the incoming
-- lists? Or can foldr fuse with consumers in ways zipWithBy can't?

-- | An unsafe variant of 'pairWithBy' to fill out the interface.
zipWithBy :: (a -> b -> c)       -- tuple homomorphism
          -> (c -> d -> d) -> d  -- list  homomorphism
          -> [a] -> [b] -> d     -- a @zip@ function

zipWithBy k f z xs' ys' = zipWB xs' ys' id
    where
    zipWB (x:xs) (y:ys) cc = zipWB xs ys (cc . f (k x y))
    zipWB _      _      cc = cc z


-- | A version of 'zip' that uses a user-defined list homomorphism.
zipBy :: ((a,b) -> m (a,b) -> m (a,b)) -> m (a,b)
      -> [a] -> [b] -> m (a,b)
zipBy  = zipWithBy (,)


----------------------------------------------------------------
----------------------------------------------------------------
-- | A generic version of 'pair'. The first argument is a tuple
-- homomorphism (i.e. a function for how to combine values from the
-- two lists), the second two arguments form a list homomorphism
-- (i.e. so you can 'foldr' the @[c]@ list directly without actually
-- constructing it).
--
-- In order to evaluate to WHNF 'pairWithBy' is strict in both list
-- arguments, as it must be, to determine that the lists are of the
-- same length. This means it can survive one infinite list (yielding
-- 'Nothing') but that it can't survive two. The implementation is
-- very efficient and uses a tight tail-recursive loop, however
-- with extremely long lists it will be churning through heap and
-- that tightness can make it hard to interrupt (lists of 1 million
-- elements return in 1~2 seconds, but lists of 10 million can lock
-- your system up).

pairWithBy :: (a -> b -> c)          -- @(,)@ tuple homomorphism
           -> (c -> d -> d)          -- @(:)@ list  homomorphism, pt. 1
           -> d                      -- @[]@  list  homomorphism, pt. 2
           -> [a] -> [b] -> Maybe d  -- a safer @zip@ function

pairWithBy k f z xs' ys' = pairWB xs' ys' id
    where
    -- N.B. Strict accumulators are usually awesome, but don't
    -- even consider it when doing CPS! Making @cc@ strict degrades
    -- performance significantly; it takes twice as long and twice
    -- as much heap just to get to WHNF. After evaluating the spine
    -- of the resulting list from 'pair' that drops to +10% time
    -- and +25% heap, which is still much worse.
    
    pairWB (x:xs) (y:ys) cc = pairWB xs ys (cc . f (k x y))
    pairWB []     []     cc = Just (cc z)
    pairWB _      _      _  = Nothing

----------------------------------------------------------------

-- | A safe version of 'zipWith'.
pairWith  :: (a -> b -> c)
          -> [a] -> [b] -> Maybe [c]
pairWith f = pairWithBy f (:) []


-- | A safe version of 'zip' that uses a user-defined list homomorphism.
pairBy :: ((a,b) -> d -> d) -> d
       -> [a] -> [b] -> Maybe d
pairBy  = pairWithBy (,)


-- | A safe version of 'zip'.
pair :: [a] -> [b] -> Maybe [(a,b)]
pair  = pairBy (:) []


----------------------------------------------------------------
-- These two are just here because they're often requested, and
-- besides they're kinda cute.

-- | A bijection from a list of functions and a list of arguments
-- to a list of results of applying the functions bijectively.
biject :: [a -> b] -> [a] -> Maybe [b]
biject  = pairWith ($) -- 'id' also works


-- | A version of 'biject' that applies functions strictly. N.B.
-- the list is still lazily evaluated, this just makes the functions
-- strict in their argument.
biject' :: [a -> b] -> [a] -> Maybe [b]
biject'  = pairWith ($!)

----------------------------------------------------------------
----------------------------------------------------------- fin.
