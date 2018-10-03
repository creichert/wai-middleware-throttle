-----------------------------------------------------------------------------
-- |
-- Module      : Network.Wai.Middleware.Throttle.Internal
-- Description : WAI Request Throttling Middleware
-- Copyright   : (c) 2018 Christopher Reichert, Daniel Fithian
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com> Daniel Fithian <daniel.m.fithian@gmail.com>
-- Stability   : experimental
-- Portability : POSIX
--
-- Uses a <https://en.wikipedia.org/wiki/Token_bucket Token Bucket>
-- algorithm (from the token-bucket package) to throttle WAI Requests.
--
--
-- == Example
--
-- @
-- main = do
--   let expirationSpec = TimeSpec 5 0 -- five seconds
--   th <- initThrottler (defaultThrottleSettings expirationSpec) (const $ Right 1)
--   let appl = throttle th $ \ _ f -> f $
--         responseLBS ok200 [] "ok"
--   Warp.run 3000 app
-- @
module Network.Wai.Middleware.Throttle
  ( module Network.Wai.Middleware.Throttle.Internal
  ) where

import Network.Wai.Middleware.Throttle.Internal
  ( Throttle (..)
  , ThrottleSettings (..)
  , Address (..)
  , defaultThrottleSettings
  , extractAddress
  , initThrottler
  , throttle )
