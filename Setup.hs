#!/usr/bin/env runhaskell

module Main (main) where

-- <http://www.haskell.org/ghc/docs/latest/html/libraries/Cabal/Distribution-Simple.html>
import Distribution.Simple

{-
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Exit
import System.Cmd
import System.Directory
import Control.Exception

withCurrentDirectory       :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)


-- <http://blog.holdenkarau.com/2008/07/integrating-your-hunit-or-other-tests.html>
runTestScript :: Args
              -> Bool
              -> PackageDescription
              -> LocalBuildInfo
              -> IO ExitCode
runTestScript args flag pd lbi = withCurrentDirectory "src" (system "make")


main :: IO ()
main  = defaultMainWithHooks defaultUserHooks {runTests = runTestScript}
-}

main :: IO ()
main  = defaultMain
