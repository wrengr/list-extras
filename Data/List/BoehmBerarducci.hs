
{-# LANGUAGE CPP, Rank2Types #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

-- Unfortunately GHC < 6.10 needs -fglasgow-exts in order to actually
-- parse RULES (see -ddump-rules); the -frewrite-rules flag only
-- enables the application of rules, instead of doing what we want.
-- Apparently this is fixed in 6.10.
--
-- http://hackage.haskell.org/trac/ghc/ticket/2213
-- http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg14313.html
{-# OPTIONS_GHC -O2 -fglasgow-exts -frewrite-rules #-}

----------------------------------------------------------------
--                                                  ~ 2012.09.19
-- |
-- Module      :  Data.List.BoehmBerarducci
-- Copyright   :  Copyright (c) 2010--2012 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  semi-portable (CPP, Rank2Types)
--
-- This module defines the Boehm--Berarducci encoding of lists.
-- These are provided by numerous other libraries, but we offer it
-- here for API similarity to "Data.List.Scott". This encoding is
-- often confused with the Church encoding; for more discussion on
-- the differences between this encoding and Church encoding, see
-- <http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html>.
-- Implementations of functions not in "Data.List.Scott" were taken
-- from <http://okmij.org/ftp/Algorithms.html#zip-folds>
--
-- Functions marked as /O(L)/ are /O(1)/ in a lazy setting, but are
-- /O(n)/ in an eager setting. And functions marked /O(S)/ are
-- linear time, but more importantly will result in the loss of
-- sharing.
----------------------------------------------------------------
module Data.List.BoehmBerarducci where

import Prelude hiding (mapM, sequence, foldr, foldr1, foldl, foldl1)
import qualified Prelude

import Data.Foldable
import Data.Traversable
import Data.Monoid

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#endif
----------------------------------------------------------------
----------------------------------------------------------------

-- | A Boehm--Berarducci-encoded list. The Boehm--Berarducci encoding
-- of a datatype is its catamorphic elimination.
newtype BoehmBerarducciList a =
    BBL { cataBBL :: forall r. (a -> r -> r) -> r -> r }


-- | /O(1)/. The empty list.
nilBBL :: BoehmBerarducciList a
nilBBL = BBL $ \_c n -> n


-- | /O(S)/. Add an element to the front of the list.
consBBL :: a -> BoehmBerarducciList a -> BoehmBerarducciList a
consBBL x xs = BBL $ \c n -> c x (cataBBL xs c n)


-- | /O(n)/. The right fold eliminator.
foldrBBL :: (a -> b -> b) -> b -> BoehmBerarducciList a -> b
foldrBBL c n = \xs -> cataBBL xs c n


-- | /O(S)/. Case analysis.
caseBBL :: BoehmBerarducciList a -> b -> (a -> BoehmBerarducciList a -> b) -> b
caseBBL xs n c =
    -- Ugh, is this is the best we can do?
    case boehmBerarducciToList xs of
    []    -> n
    x:xs' -> c x (boehmBerarducciFromList xs')


-- | /O(n)/. Convert to plain lists.
boehmBerarducciToList :: BoehmBerarducciList a -> [a]
#ifdef __GLASGOW_HASKELL__
boehmBerarducciToList xs = build (cataBBL xs)
#else
boehmBerarducciToList xs = cataBBL xs (:) []
#endif


-- | /O(n)/. Convert from plain lists.
boehmBerarducciFromList :: [a] -> BoehmBerarducciList a
boehmBerarducciFromList = Prelude.foldr consBBL nilBBL

----------------------------------------------------------------
instance (Show a) => Show (BoehmBerarducciList a) where
    show xs = "BBL " ++ show (boehmBerarducciToList xs)

instance (Eq a) => Eq (BoehmBerarducciList a) where
    xs == ys =
        boehmBerarducciToList xs == boehmBerarducciToList ys

instance (Ord a) => Ord (BoehmBerarducciList a) where
    -- TODO: Is there a more efficient implementation?
    xs `compare` ys =
        boehmBerarducciToList xs `compare` boehmBerarducciToList ys

instance Functor BoehmBerarducciList where
    fmap f xs = BBL $ \c n -> cataBBL xs (c . f) n

----------------------------------------------------------------

-- | /O(L)/. Return whether the list is empty.
nullBBL :: BoehmBerarducciList a -> Bool
nullBBL xs = cataBBL xs (\_ _ -> False) True


-- | /O(L)/. Return the first element in a list, if any exists.
headBBL :: BoehmBerarducciList a -> Maybe a
headBBL xs = cataBBL xs (\hd _ -> Just hd) Nothing


-- | /O(n)/. Drop the first element in a list, if any exists.
tailBBL :: BoehmBerarducciList a -> Maybe (BoehmBerarducciList a)
tailBBL xs
    | nullBBL ys = Nothing
    | otherwise  = Just ys
    where
    ys = drop1BBL xs


-- | /O(n)/. Drop the first element in a list.
drop1BBL :: BoehmBerarducciList a -> BoehmBerarducciList a
drop1BBL xs =
    fst $ cataBBL xs (\x (_,ys) -> (ys, consBBL x ys)) (nilBBL,nilBBL)


-- | /O(n)/. Drop the last element in a list.
init1BBL :: BoehmBerarducciList a -> BoehmBerarducciList a
init1BBL xs = cataBBL xs (\x tl k -> k (tl (consBBL x))) (\_ -> nilBBL) id


-- | /O(n+m)/. Append two lists.
appendBBL :: BoehmBerarducciList a -> BoehmBerarducciList a -> BoehmBerarducciList a
appendBBL xs ys = BBL $ \c n -> cataBBL xs c (cataBBL ys c n)


zipBBL :: BoehmBerarducciList a -> BoehmBerarducciList b -> BoehmBerarducciList (a,b)
zipBBL = zipWithBBL (,)


----------------------------------------------------------------
-- <http://okmij.org/ftp/Haskell/zip-folds.lhs>

zipWithBBL :: (a -> b -> c) -> BoehmBerarducciList a -> BoehmBerarducciList b -> BoehmBerarducciList c
zipWithBBL f xs ys =
    BBL $ \c n -> 
        cataBBL xs
            (\x r xs' -> cataBBL xs' (\y _ -> c (f x y) (r (drop1BBL xs'))) n)
            (const n) ys


filterBBL :: (a -> Bool) -> BoehmBerarducciList a -> BoehmBerarducciList a
filterBBL p xs =
    BBL $ \c -> cataBBL xs (\x r -> if p x then c x r else r)


takeWhileBBL :: (a -> Bool) -> BoehmBerarducciList a -> BoehmBerarducciList a
takeWhileBBL p xs =
    BBL $ \c n -> cataBBL xs (\x r -> if p x then c x r else n) n


takeBBL :: (Ord n, Num n) => n -> BoehmBerarducciList a -> BoehmBerarducciList a
takeBBL n xs =
    BBL $ \c z -> cataBBL xs
        (\x r n' -> if n' <= 0 then z else c x (r (n'-1)))
        (const z) n


-- /O(n)/.
dropBBL :: (Ord n, Num n) => n -> BoehmBerarducciList a -> BoehmBerarducciList a
dropBBL n xs = 
    BBL $ \c z -> cataBBL xs
        (\x r n' -> if n' <= 0 then c x (r n') else r (n'-1))
        (const z) n


dropWhileBBL :: (a -> Bool) -> BoehmBerarducciList a -> BoehmBerarducciList a
dropWhileBBL p xs =
    BBL $ \c n -> cataBBL xs
        (\x r done ->
            if done then c x (r done)
            else if p x then r done
            else c x (r True))
        (const n) False

----------------------------------------------------------------
----------------------------------------------------------- fin.
