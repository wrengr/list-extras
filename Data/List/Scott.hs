
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
--                                                  ~ 2011.06.23
-- |
-- Module      :  Data.List.Scott
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  semi-portable (CPP, Rank2Types)
--
-- This module defines the Scott encoding of lists. While Church
-- encodings are more popular, Scott encodings offer a number of
-- benefits and deserve to be more widely used. In particular, Scott
-- encodings are /exact/; meaning that all operations on the encoding
-- can be performed with the same complexity as on the data type
-- (with a larger or smaller constant factor, depending on how
-- efficiently the compiler handles functions vs structures). And
-- since the conversion from the Scott encoding to case-elimination
-- form is immediate, using Scott encodings should facilitate fusion
-- without special support for optimizing away allocations that
-- will be immediately case-analyzed (only support for fusing and
-- partially evaluating functions is necessary, and should already
-- be available).
--
-- Thus,
-- Scott encodings provide an alternative to having case analysis
-- built into the language. On the other hand, Scott encodings
-- require the language to support term- and type-level recursive
-- bindings, whereas Church encodings do not. Also, because they
-- are inexact, Church encodings can optimize certain operations
-- (like concatenation) but must pessimize other operations (like
-- case analysis, 'tail', etc).
----------------------------------------------------------------
module Data.List.Scott where

import Prelude hiding (mapM, sequence, foldr, foldr1, foldl, foldl1)
import qualified Prelude

import Data.Or
import Data.Foldable
import Data.Traversable
import Data.Monoid

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Exts (build)
#endif
----------------------------------------------------------------
----------------------------------------------------------------

-- | A Scott-encoded list. The Scott encoding of a datatype is its
-- case-analysis elimination.
newtype ScottList a =
    SL { caseSL :: forall r. r -> (a -> ScottList a -> r) -> r }

-- | /O(1)/. The empty list.
nilSL :: ScottList a
nilSL = SL const

-- | /O(1)/. Add an element to the front of the list.
consSL :: a -> ScottList a -> ScottList a
consSL x xs = SL $ \_n c -> c x xs

-- | /O(n)/. The right fold eliminator.
foldrSL :: (a -> b -> b) -> b -> ScottList a -> b
foldrSL c n xs = caseSL xs n (\x xs' -> c x (foldrSL n c xs'))
{-# INLINE [0] foldrSL #-}

-- | /O(n)/. Convert to plain lists.
scottToList :: ScottList a -> [a]
#ifdef __GLASGOW_HASKELL__
scottToList = build scottFoldr
#else
scottToList = scottFoldr (:) []
#endif

-- | /O(n)/. Convert from plain lists.
scottFromList :: [a] -> ScottList a
scottFromList = Prelude.foldr consSL nilSL

----------------------------------------------------------------

instance (Show a) => Show (ScottList a) where
    show xs = "SL " ++ show (scottToList xs)

instance (Eq a) => Eq (ScottList a) where
    xs == ys = scottToList xs == scottToList ys

instance (Ord a) => Ord (ScottList a) where
    -- TODO: implement directly via bifoldlSL/bifoldl'SL
    xs `compare` ys = scottToList xs `compare` scottToList ys

instance Functor ScottList where
    -- TODO: Is there a more efficient implementation?
    fmap f = foldrSL (consSL . f) nilSL

----------------------------------------------------------------

-- | /O(1)/. Return the first element in a stream, if any exists.
headSL :: ScottList a -> Maybe a
headSL xs = caseSL xs Nothing (\hd _ -> Just hd)

-- | /O(1)/. Drop the first element in a stream, if any exists.
tailSL :: ScottList a -> Maybe (ScottList a)
tailSL xs = caseSL xs Nothing (\_ tl -> Just tl)

-- | /O(1)/. Drop the first element in a stream.
drop1SL :: ScottList a -> ScottList a
drop1SL xs = caseSL xs nilSL (\_ tl -> tl)

foldlSL :: (b -> a -> b) -> b -> ScottList a -> b
foldlSL s n xs = caseSL xs n (\x xs' -> foldlSL s (s n x) xs')

foldl'SL :: (b -> a -> b) -> b -> ScottList a -> b
foldl'SL s n xs = caseSL xs n (\x xs' -> (foldl'SL s $! s n x) xs')

appendSL :: ScottList a -> ScottList a -> ScottList a
appendSL xs ys = foldrSL consSL ys xs
    -- TODO: Is there a more efficient implementation?

zipSL :: ScottList a -> ScottList b -> ScottList (a,b)
zipSL = zipWithSL (,)

zipWithSL :: (a -> b -> c) -> ScottList a -> ScottList b -> ScottList c
zipWithSL f = bifoldrSL phi nilSL
    where
    phi (Both x y) zs = consSL (f x y) zs
    phi _          _  = nilSL

bifoldrSL :: (Or a b -> c -> c) -> c -> ScottList a -> ScottList b -> c
bifoldrSL k z = go
    where
    go xs ys =
        caseSL xs
            (caseSL ys
                z
                (\y ys' -> k (Snd y) (foldrSL (k . Snd) z ys'))
            (\x xs' ->
                (caseSL ys
                    (k (Fst x) (foldrSL (k . Fst) z xs'))
                    (\y ys' -> k (Both x y) (go xs' ys'))))

bifoldlSL :: (c -> Or a b -> c) -> c -> ScottList a -> ScottList b -> c
bifoldlSL k z xs ys =
    caseSL xs
        (caseSL ys
            z
            (\y ys' -> foldlSL (\z' y' -> k z' $ Snd y') (k z $ Snd y) ys'))
        (\x xs' ->
            (caseSL ys
                (foldlSL (\z' x' -> k z' $ Fst x') (k z $ Fst x) xs')
                (\y ys' -> bifoldlSL k (k z $ Both x y) xs' ys')))


----------------------------------------------------------------
----------------------------------------------------------- fin.
