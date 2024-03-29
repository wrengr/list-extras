{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Prelude.Listless
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  Haskell98 (+CPP)
--
-- This module provides the "Prelude" but removing all the list
-- functions. This is helpful for modules that overload those
-- function names to work for other types. Note that on GHC 7.6 and
-- above @catch@ is no longer exported from the Prelude, and also
-- not re-exported from here; whereas, on earlier versions of GHC
-- (and non-GHC compilers) we still re-export it.
--
-- Be sure to disable the implicit importing of the prelude when
-- you import this one (by passing @-fno-implicit-prelude@ for GHC,
-- or by explicitly importing the prelude with an empty import list
-- for most implementations).
----------------------------------------------------------------

module Prelude.Listless
    -- We have to manually specify the exports; can't export @module
    -- Prelude@ because that'll export everything and only hide the
    -- noted parts from this module, which is meaningless.
    (($!), ($), (&&), (.), (=<<), Bool(..), Bounded(..), Char,
    Double, Either(..), Enum(..), Eq(..), FilePath, Float, Floating(..),
    Fractional(..), Functor(..), IO, IOError, Int, Integer,
    Integral(..), Maybe(..), Monad(..), Num(..), Ord(..), Ordering(..),
    Rational, Read(..), ReadS, Real(..), RealFloat(..), RealFrac(..),
    Show(..), ShowS, String, (^), (^^), appendFile, asTypeOf,
#if __GLASGOW_HASKELL__ < 706
    catch,
#endif
    const, curry, either, error, even, flip, fromIntegral, fst,
    gcd, getChar, getContents, getLine, id, interact, ioError, lcm,
    lex, maybe, not, odd, otherwise, print, putChar, putStr, putStrLn,
    read, readFile, readIO, readLn, readParen, reads, realToFrac,
    seq, showChar, showParen, showString, shows, snd, subtract,
    uncurry, undefined, until, userError, writeFile, (||))

    where

import Prelude hiding
    ((!!), (++), all, and, any, break, concat, concatMap, cycle,
    drop, dropWhile, elem, filter, foldl, foldl1, foldr, foldr1,
    head, init, iterate, last, length, lines, lookup, map, mapM,
    mapM_, maximum, minimum, notElem, null, or, product, repeat,
    replicate, reverse, scanl, scanl1, scanr, scanr1, sequence,
    sequence_, span, splitAt, sum, tail, take, takeWhile, unlines,
    unwords, unzip, unzip3, words, zip, zip3, zipWith, zipWith3)

----------------------------------------------------------------
----------------------------------------------------------- fin.
