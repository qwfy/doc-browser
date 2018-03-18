module Main (main) where

import Control.Concurrent.STM.TMVar
import Control.Monad.STM

import System.Directory

import qualified Search
import qualified DevDocs
import qualified Entry
import Utils

import Criterion.Main

main :: IO ()
main = do
  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  allEntries <- DevDocs.loadAll configRoot

  -- start a dedicated thread
  -- communicate the search thread with a TMVar
  -- wait for result
  matchesSlot <- atomically newEmptyTMVar
  querySlot <- atomically newEmptyTMVar

  let handleMatches ms =
        atomically $ putTMVar matchesSlot ms

  _ <- Search.startThread
    7701
    configRoot
    (Entry.toMatch 7701)
    allEntries
    Nothing
    querySlot
    handleMatches

  let searchDedicated c n = do
        let str = replicate n c
        atomically $ updateTMVar querySlot str
        atomically $ takeTMVar matchesSlot

  defaultMain
    [ bgroup "search_dedicated"
        [ bench "1"    $ nfIO (searchDedicated 'a' 1)
        , bench "2"    $ nfIO (searchDedicated 'a' 2)
        , bench "3"    $ nfIO (searchDedicated 'a' 3)
        , bench "4"    $ nfIO (searchDedicated 'a' 4)
        , bench "5"    $ nfIO (searchDedicated 'a' 5)
        , bench "6"    $ nfIO (searchDedicated 'a' 6)
        , bench "7"    $ nfIO (searchDedicated 'a' 7)
        , bench "8"    $ nfIO (searchDedicated 'a' 8)
        , bench "9"    $ nfIO (searchDedicated 'a' 9)
        , bench "11"   $ nfIO (searchDedicated 'a' 11)
        , bench "13"   $ nfIO (searchDedicated 'a' 13)
        , bench "15"   $ nfIO (searchDedicated 'a' 15)
        , bench "17"   $ nfIO (searchDedicated 'a' 17)
        , bench "19"   $ nfIO (searchDedicated 'a' 19)
        ]
    ]
