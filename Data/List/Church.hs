
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
--                                                  ~ 2012.07.19
-- |
-- Module      :  Data.List.Church
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  semi-portable (CPP, Rank2Types)
--
-- This module defines the Church encoding of lists. These are
-- provided by numerous other libraries, but we offer it here for
-- API similarity to "Data.List.Scott".
----------------------------------------------------------------
module Data.List.Church where

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

-- | A Church-encoded list. The Church encoding of a datatype is
-- its catamorphic elimination.
newtype ChurchList a =
    CL { cataCL :: forall r. (a -> r -> r) -> r -> r }

nilCL :: ChurchList a
nilCL = CL $ \_c n -> n

consCL :: a -> ChurchList a -> ChurchList a
consCL x xs = CL $ \c n -> c x (cataCL xs c n)

foldrCL :: (a -> b -> b) -> b -> ChurchList a -> b
foldrCL c n = \xs -> cataCL xs c n

caseCL :: ChurchList a -> b -> (a -> ChurchList a -> b) -> b
caseCL xs n c =
    case churchToList xs of
    []    -> n
    x:xs' -> c x (churchFromList xs')
    -- Ugh, this is the best we can do.

churchToList :: ChurchList a -> [a]
#ifdef __GLASGOW_HASKELL__
churchToList xs = build (cataCL xs)
#else
churchToList xs = cataCL xs (:) []
#endif

churchFromList :: [a] -> ChurchList a
churchFromList = Prelude.foldr consCL nilCL

----------------------------------------------------------------
instance (Show a) => Show (ChurchList a) where
    show xs = "CL " ++ show (churchToList xs)

instance (Eq a) => Eq (ChurchList a) where
    xs == ys = churchToList xs == churchToList ys

instance (Ord a) => Ord (ChurchList a) where
    -- TODO: Is there a more efficient implementation?
    xs `compare` ys = churchToList xs `compare` churchToList ys

instance Functor ChurchList where
    fmap f xs = CL $ \c n -> cataCL xs (c . f) n

----------------------------------------------------------------

-- | Return the first element in a stream, if any exists.
headCL :: ChurchList a -> Maybe a
headCL xs = cataCL xs (\hd _ -> Just hd) Nothing

{-
-- BUG: less polymorphic than expected. Fixing that'll mean refolding the tail of the list.
-- | Drop the first element in a stream, if any exists.
tailCL :: ChurchList a -> Maybe (ChurchList a)
tailCL xs = cataCL xs (\_ tl -> Just (CL tl)) Nothing

-- BUG: less polymorphic than expected. Fixing that'll mean refolding the tail of the list.
-- | Drop the first element in a stream.
drop1CL :: ChurchList a -> ChurchList a
drop1CL xs = cataCL xs (\_ tl -> CL tl) nilCL
-}

init1CL :: ChurchList a -> ChurchList a
init1CL xs = cataCL xs (\x tl k -> k (tl (consCL x))) (\_ -> nilCL) id

appendCL :: ChurchList a -> ChurchList a -> ChurchList a
appendCL xs ys = CL $ \c n -> cataCL xs c (cataCL ys c n)

zipSL :: ChurchList a -> ChurchList b -> ChurchList (a,b)
zipSL = zipWithSL (,)

zipWithSL :: (a -> b -> c) -> ChurchList a -> ChurchList b -> ChurchList c
zipWithSL xs ys = undefined


----------------------------------------------------------------
----------------------------------------------------------- fin.
