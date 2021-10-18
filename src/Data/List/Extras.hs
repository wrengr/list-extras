{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.List.Extras
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  Haskell98
--
-- This module provides a single header for all @Data.List.Extras.*@
-- modules and provides a small number of other utility functions.
----------------------------------------------------------------

module Data.List.Extras
    (
      list
    , module Data.List.Extras.LazyLength
    , module Data.List.Extras.Pair
    , module Data.List.Extras.Argmax
    ) where

import Data.List.Extras.LazyLength
import Data.List.Extras.Pair
import Data.List.Extras.Argmax

-- | Pattern matching for lists, as a first-class function. (Could
-- also be considered as a non-recursive 'foldr'.) If the list
-- argument is @[]@ then the default argument is returned; otherwise
-- the function is called with the head and tail of the list.
list :: (a -> [a] -> b) -> b -> [a] -> b
list _ z []     = z
list f _ (x:xs) = f x xs

----------------------------------------------------------------
----------------------------------------------------------- fin.
