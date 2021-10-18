
{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-binds #-}

----------------------------------------------------------------
--                                                  ~ 2008.07.20
-- |
-- Module      :  Data.List.Extras.ArgMax
-- Copyright   :  Copyright (c) 2007--2008 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides variants of the 'maximum' and 'minimum'
-- functions which return the element for which some function is
-- maximized or minimized.
----------------------------------------------------------------

module Data.List.Extras.ArgMax
    (
    -- * Maximum variations
      argmax, argmax'
    
    -- * Minimum variations
    , argmin, argmin'
    
    -- * Generic versions
    , argmaxBy, argmaxBy'
    
    {- TODO: CPS and monadic variants; argmax2, argmax3,... -}
    {- TODO: make argmax et al "good consumers" for fusion -}
    ) where
-- argmaxM       :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (Maybe a)


----------------------------------------------------------------
----------------------------------------------------------------

-- CPS version... why bother making the list first?
type CPSList a b = ((a -> b -> b) -> b -> b)

-- BUG: This doesn't tail recurse and so it will stack overflow!!!
argmaxCPS       :: (Ord b) => (a -> b) -> CPSList a (Maybe (a,b)) -> Maybe a
argmaxCPS f list = list k Nothing >>= Just . fst
    where
    k x Nothing    = Just (x, f x)
    k x (Just yfy) = Just (mx x yfy)
    
    mx a (b,fb) = let  fa = f a in
                  if   fa > fb
                  then (a,fa)
                  else (b,fb)


----------------------------------------------------------------
----------------------------------------------------------------
-- Compared against a generic decorate-fold-undecorate implementation:
--     fst . foldr1 (\a b -> if snd a > snd b then a else b)
--         . map (\x -> (x, f x))
-- that seems to take 50% to 100% more space vs argmax'. Using
-- pattern matching instead of snd seems to improve it, but my tests
-- are a bit unclear.

emptyListError    :: String -> a
emptyListError fun = error $ "Data.List.Extras.ArgMax."++fun++": empty list"


-- | Tail-recursive driver
_argmaxBy :: (b -> b -> Bool) -> (a -> b) -> [a] -> (a,b) -> a
_argmaxBy isBetterThan f = go
    where
    go []     (b,_)  = b
    go (x:xs) (b,fb) = go xs $! cmp x (b,fb)
    
    cmp a (b,fb) = let  fa = f a in
                   if   fa `isBetterThan` fb
                   then (a,fa)
                   else (b,fb)


-- | Direct version of 'argmaxBy' which doesn't catch the empty
-- list error.
argmaxBy' :: (b -> b -> Ordering) -> (a -> b) -> [a] -> a
argmaxBy' _   _ []     = emptyListError "argmaxBy'"
argmaxBy' ord f (x:xs) = _argmaxBy boolOrd f xs (x, f x)
    where
    boolOrd a b = GT == ord a b


-- | Returns the element of the list which maximizes a function
-- according to a user-defined ordering, or @Nothing@ if the list
-- was empty.
argmaxBy :: (b -> b -> Ordering) -> (a -> b) -> [a] -> Maybe a
argmaxBy _   _ []       = Nothing
argmaxBy ord f xs@(_:_) = Just (argmaxBy' ord f xs)


----------------------------------------------------------------
-- The specialization pragma add about 21kB to the library size
-- (compared to 29kB base for list-extras). For a basic utility
-- library that's a bit excessive, though if we break the argmax
-- stuff out from list-extras then we might go through with it for
-- performance sake.

-- | Direct version of 'argmax' which doesn't catch the empty list
-- error.
{-
{-# SPECIALIZE argmax' :: (a -> Int)     -> [a] -> a #-}
{-# SPECIALIZE argmax' :: (a -> Integer) -> [a] -> a #-}
{-# SPECIALIZE argmax' :: (a -> Float)   -> [a] -> a #-}
{-# SPECIALIZE argmax' :: (a -> Double)  -> [a] -> a #-}
-}
argmax'                :: (Ord b) => (a -> b) -> [a] -> a
argmax' _ []     = emptyListError "argmax'"
argmax' f (x:xs) = _argmaxBy (>) f xs (x, f x)


-- | Returns the element of the list which maximizes the function,
-- or @Nothing@ if the list was empty.
{-
{-# SPECIALIZE argmax :: (a -> Int)     -> [a] -> Maybe a #-}
{-# SPECIALIZE argmax :: (a -> Integer) -> [a] -> Maybe a #-}
{-# SPECIALIZE argmax :: (a -> Float)   -> [a] -> Maybe a #-}
{-# SPECIALIZE argmax :: (a -> Double)  -> [a] -> Maybe a #-}
-}
argmax                :: (Ord b) => (a -> b) -> [a] -> Maybe a
argmax _ []       = Nothing
argmax f xs@(_:_) = Just (argmax' f xs)


----------------------------------------------------------------

-- | Direct version of 'argmin' which doesn't catch the empty list
-- error.
{-
{-# SPECIALIZE argmin' :: (a -> Int)     -> [a] -> a #-}
{-# SPECIALIZE argmin' :: (a -> Integer) -> [a] -> a #-}
{-# SPECIALIZE argmin' :: (a -> Float)   -> [a] -> a #-}
{-# SPECIALIZE argmin' :: (a -> Double)  -> [a] -> a #-}
-}
argmin'                :: (Ord b) => (a -> b) -> [a] -> a
argmin' _ []     = emptyListError "argmin'"
argmin' f (x:xs) = _argmaxBy (<) f xs (x, f x)


-- | Returns the element of the list which minimizes the function,
-- or @Nothing@ if the list was empty.
{-
{-# SPECIALIZE argmin :: (a -> Int)     -> [a] -> Maybe a #-}
{-# SPECIALIZE argmin :: (a -> Integer) -> [a] -> Maybe a #-}
{-# SPECIALIZE argmin :: (a -> Float)   -> [a] -> Maybe a #-}
{-# SPECIALIZE argmin :: (a -> Double)  -> [a] -> Maybe a #-}
-}
argmin                :: (Ord b) => (a -> b) -> [a] -> Maybe a
argmin _ []       = Nothing
argmin f xs@(_:_) = Just (argmin' f xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.
