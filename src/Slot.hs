{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Slot
  ( T(..)
  , Query(..)
  , empty
  ) where

import Control.Concurrent.STM

import qualified Match

data Query
  = GuiQuery String
  | HttpQuery String (TMVar [Match.T])

data T = T
  { query            :: TMVar Query
  , summon       :: TMVar String
  }

empty :: STM T
empty = do
  query <- newEmptyTMVar
  summon <- newEmptyTMVar
  return $ T {query, summon}
