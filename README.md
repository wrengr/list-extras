list-extras
===========
[![Hackage version](https://img.shields.io/hackage/v/list-extras.svg?style=flat)](https://hackage.haskell.org/package/list-extras)
[![Build Status](https://github.com/wrengr/list-extras/workflows/ci/badge.svg)](https://github.com/wrengr/list-extras/actions?query=workflow%3Aci)
[![Dependencies](https://img.shields.io/hackage-deps/v/list-extras.svg?style=flat)](http://packdeps.haskellers.com/specific?package=list-extras)

The list-extras package provides a few common not-so-common functions
for lists.

## Install

This is a simple package and should be easy to install.  You should
be able to use one of the following standard methods to install it.

    -- With cabal-install and without the source:
    $> cabal install list-extras

    -- With cabal-install and with the source already:
    $> cd list-extras
    $> cabal install

    -- Without cabal-install, but with the source already:
    $> cd list-extras
    $> runhaskell Setup.hs configure --user
    $> runhaskell Setup.hs build
    $> runhaskell Setup.hs haddock --hyperlink-source
    $> runhaskell Setup.hs copy
    $> runhaskell Setup.hs register

The Haddock step is optional.


## Portability

The implementation is quite portable, relying only on a few basic
language extensions. The complete list of extensions used is:

* CPP
* Rank2Types
* ExistentialQuantification

## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/list-extras)
* [GitHub](https://github.com/wrengr/list-extras)
