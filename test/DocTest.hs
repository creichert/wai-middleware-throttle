-----------------------------------------------------------------------------
-- |
-- Module      : DocTest.hs
-- Description : Documentation Testing
-- Copyright   : (c) 2015 Christopher Reichert
-- License     : AllRightsReserved
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : testing
-- Portability : POSIX


module Main (main) where


import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)


main :: IO ()
main = glob "Network/**/[A-Z]*.hs"
         >>= doctest
