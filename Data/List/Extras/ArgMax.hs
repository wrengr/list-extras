{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                  ~ 2010.10.15
-- |
-- Module      :  Data.List.Extras.ArgMax
-- Copyright   :  Copyright (c) 2007--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- This module provides variants of the 'maximum' and 'minimum'
-- functions which return the elements for which some function is
-- maximized or minimized.
----------------------------------------------------------------

module Data.List.Extras.Argmax
    (
    -- * Utility functions
      catchNull
    
    -- * Generic versions
    , argmaxBy, argmaxesBy, argmaxWithMaxBy, argmaxesWithMaxBy
    
    -- * Maximum variations
    , argmax,   argmaxes,   argmaxWithMax,   argmaxesWithMax
    
    -- * Minimum variations
    , argmin,   argmins,    argminWithMin,   argminsWithMin
    
    {- TODO: CPS and monadic variants; argmax2, argmax3,... -}
    {- TODO: make sure argmax et al are "good consumers" for fusion -}
    ) where
-- argmaxM       :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (Maybe a)

import Data.List (foldl')

----------------------------------------------------------------
----------------------------------------------------------------

-- | Apply a list function safely, i.e. when the list is non-empty.
-- All other functions will throw errors on empty lists, so use
-- this to make your own safe variations.
catchNull :: ([a] -> b) -> ([a] -> Maybe b)
{-# INLINE catchNull #-}
-- We use the explicit lambda in order to improve inlining in ghc-7.
catchNull f = \xs ->
    case xs of
    []  -> Nothing
    _:_ -> Just (f xs)


-- | Minimize the number of string literals
emptyListError :: String -> a
{-# NOINLINE emptyListError #-}
emptyListError fun =
    error $ "Data.List.Extras.Argmax."++fun++": empty list"


-- | Apply a list function unsafely. For internal use.
throwNull :: String -> (a -> [a] -> b) -> ([a] -> b)
{-# INLINE throwNull #-}
-- We use the explicit lambda in order to improve inlining in ghc-7.
throwNull fun f = \xs ->
    case xs of
    []    -> emptyListError fun
    x:xs' -> f x xs'

----------------------------------------------------------------
----------------------------------------------------------------
-- | Tail-recursive driver
_argmaxWithMaxBy :: (b -> b -> Bool) -> (a -> b) -> a -> [a] -> (a,b)
{-# INLINE _argmaxWithMaxBy #-}
-- We use the explicit lambda in order to improve inlining in ghc-7.
_argmaxWithMaxBy isBetterThan f =
    \x xs -> foldl' cmp (x, f x) xs
    where
    cmp bfb@(_,fb) a =
        let  fa = f a in
        if   fa `isBetterThan` fb
        then (a,fa)
        else bfb


-- | Tail-recursive driver
_argmaxesWithMaxBy :: (b -> b -> Ordering) -> (a -> b) -> a -> [a] -> ([a],b)
{-# INLINE _argmaxesWithMaxBy #-}
-- We use the explicit lambda in order to improve inlining in ghc-7.
_argmaxesWithMaxBy isBetterEqualThan f =
    \x xs -> foldl' cmp ([x], f x) xs
    where
    cmp bsfb@(bs,fb) a =
        let  fa = f a in
        case isBetterEqualThan fa fb of
             GT -> ([a],  fa)
             EQ -> (a:bs, fb)
             _  -> bsfb


_argmaxBy :: (b -> b -> Bool) -> (a -> b) -> a -> [a] -> a
{-# INLINE _argmaxBy #-}
-- We use the point-free style in order to improve inlining in ghc-7.
_argmaxBy k f = (fst .) . _argmaxWithMaxBy k f


_argmaxesBy :: (b -> b -> Ordering) -> (a -> b) -> a -> [a] -> [a]
{-# INLINE _argmaxesBy #-}
-- We use the point-free style in order to improve inlining in ghc-7.
_argmaxesBy k f = (fst .) . _argmaxesWithMaxBy k f

----------------------------------------------------------------
----------------------------------------------------------------

bool    :: (a -> a -> Ordering) -> (a -> a -> Bool)
bool ord = \a b -> ord a b == GT


-- | Return an element of the list which maximizes the function
-- according to a user-defined ordering.
argmaxBy        :: (b -> b -> Ordering) -> (a -> b) -> [a] -> a
argmaxBy   ord f = throwNull "argmaxBy"
                 $ _argmaxBy (bool ord) f


-- | Return all elements of the list which maximize the function
-- according to a user-defined ordering.
argmaxesBy      :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
argmaxesBy ord f = throwNull "argmaxesBy"
                 $ _argmaxesBy ord f


-- | Return an element of the list which maximizes the function
-- according to a user-defined ordering, and return the value of
-- the function at that element as well.
argmaxWithMaxBy        :: (b -> b -> Ordering) -> (a -> b) -> [a] -> (a, b)
argmaxWithMaxBy   ord f = throwNull "argmaxWithMaxBy" 
                        $ _argmaxWithMaxBy (bool ord) f


-- | Return all elements of the list which maximize the function
-- according to a user-defined ordering, and return the value of
-- the function at those elements as well.
argmaxesWithMaxBy      :: (b -> b -> Ordering) -> (a -> b) -> [a] -> ([a], b)
argmaxesWithMaxBy ord f = throwNull "argmaxesWithMaxBy"
                        $ _argmaxesWithMaxBy ord f

----------------------------------------------------------------
-- SPECIALIZE on b \in {Int,Integer,Float,Double} for the four
-- functions below nearly doubles the library size (about +21kB).
-- For a basic utility library that's a bit excessive, though if
-- we break the argmax stuff out from list-extras then we might go
-- through with it for performance sake.

-- | Return an element of the list which maximizes the function.
argmax    :: (Ord b) => (a -> b) -> [a] -> a
argmax   f = throwNull "argmax"
           $ _argmaxBy (>) f

-- | Return all elements of the list which maximize the function.
argmaxes  :: (Ord b) => (a -> b) -> [a] -> [a]
argmaxes f = throwNull "argmaxes"
           $ _argmaxesBy compare f


-- | Return an element of the list which maximizes the function,
-- and return the value of the function at that element as well.
argmaxWithMax    :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmaxWithMax   f = throwNull "argmaxWithMax" 
                  $ _argmaxWithMaxBy (>) f


-- | Return all elements of the list which maximize the function,
-- and return the value of the function at those elements as well.
argmaxesWithMax  :: (Ord b) => (a -> b) -> [a] -> ([a], b)
argmaxesWithMax f = throwNull "argmaxesWithMax"
                  $ _argmaxesWithMaxBy compare f

----------------------------------------------------------------

-- | Return an element of the list which minimizes the function.
argmin   :: (Ord b) => (a -> b) -> [a] -> a
argmin  f = throwNull "argmax"
          $ _argmaxBy (<) f

-- | Return all elements of the list which minimize the function.
argmins  :: (Ord b) => (a -> b) -> [a] -> [a]
argmins f = throwNull "argmins"
          $ _argmaxesBy (flip compare) f


-- | Return an element of the list which minimizes the function,
-- and return the value of the function at that element as well.
argminWithMin   :: (Ord b) => (a -> b) -> [a] -> (a, b)
argminWithMin  f = throwNull "argminWithMin"
                 $ _argmaxWithMaxBy (<) f

-- | Return all elements of the list which minimize the function,
-- and return the value of the function at those elements as well.
argminsWithMin  :: (Ord b) => (a -> b) -> [a] -> ([a], b)
argminsWithMin f = throwNull "argminsWithMin"
                 $ _argmaxesWithMaxBy (flip compare) f

----------------------------------------------------------------
----------------------------------------------------------- fin.
