-----------------------------------------------------------------------------
-- |
-- Module      : HLint
-- Description : Lint Testing Coverage
-- Copyright   : (c) 2015 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : testing
-- Portability : POSIX

module Main (main) where

import           Language.Haskell.HLint (hlint)
import           System.Exit            (exitFailure, exitSuccess)


main :: IO ()
main = do
    hints <- hlint arguments
    print hints
    if null hints
       then exitSuccess
       else exitFailure


arguments :: [String]
arguments =
      [
        "Network"
      , "test"
      ]
