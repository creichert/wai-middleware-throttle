-----------------------------------------------------------------------------
-- |
-- Module      : Haddock
-- Description : Haddock Test Coverage
-- Copyright   : (c) 2015 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : testing
-- Portability : POSIX

module Main (main) where

import           Data.List      (genericLength)
import           Data.Maybe     (catMaybes)
import           System.Exit    (exitFailure, exitSuccess)
import           System.Process (readProcess)
import           Text.Regex     (matchRegex, mkRegex)


main :: IO ()
main = do
  output <- readProcess "cabal"
            [
              "haddock"
            , "-v"
            , "--hoogle"
            , "--html"
            , "--internal"
            ] ""

  if average (match output) >= expected
    then exitSuccess
    else putStr output >> exitFailure


-- | Expected percentage of documentation to pass
-- the test
expected :: Double
expected = 50.0


average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs


match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
