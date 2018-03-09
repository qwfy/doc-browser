module Main (main) where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Control.Concurrent

import System.Directory

import qualified Search
import qualified Devdocs
import Utils

import Criterion.Main

main :: IO ()
main = do
  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  allEntries <- DevDocs.loadAll configRoot
  allEntriesTVar <- atomically $ newTVar allEntries

  let makeQuery c n =
        let str = replicate n c
            Just query = Search.makeQuery str
        in query

  let search c n =
        let q = makeQuery c n
        in Search.search allEntries 27 q

  -- searchFork and searchDedicated compares the speed
  -- of forkIO + readTVarIO and TMVar communication

  -- start a new thread for every search
  -- read in all entries
  -- search all entries
  -- wait for result
  matchesSlot1 <- atomically newEmptyTMVar
  let searchFork c n = do
        _ <- forkIO $ do
            entries <- readTVarIO allEntriesTVar
            let q = makeQuery c n
            let matches = Search.search entries 27 q
            atomically $ putTMVar matchesSlot1 matches

        atomically $ takeTMVar matchesSlot1

  -- start a dedicated thread
  -- communicate the search thread with a TMVar
  -- wait for result
  matchesSlot2 <- atomically newEmptyTMVar
  querySlot <- atomically newEmptyTMVar

  let handleEntries entries =
        atomically $ putTMVar matchesSlot2 entries

  _ <- Search.startThread allEntries querySlot handleEntries

  let searchDedicated c n = do
        let str = replicate n c
        atomically $ updateTMVar querySlot str
        atomically $ takeTMVar matchesSlot2


  defaultMain
    [ bgroup "search_alone"
        [ bench "1"    $ nf (search 'a') 1
        , bench "2"    $ nf (search 'a') 2
        , bench "3"    $ nf (search 'a') 3
        , bench "4"    $ nf (search 'a') 4
        , bench "5"    $ nf (search 'a') 5
        , bench "6"    $ nf (search 'a') 6
        , bench "7"    $ nf (search 'a') 7
        , bench "8"    $ nf (search 'a') 8
        , bench "9"    $ nf (search 'a') 9
        , bench "11"   $ nf (search 'a') 11
        , bench "13"   $ nf (search 'a') 13
        , bench "15"   $ nf (search 'a') 15
        , bench "17"   $ nf (search 'a') 17
        , bench "19"   $ nf (search 'a') 19
        , bench "17 b" $ nf (search 'b') 17
        , bench "19 b" $ nf (search 'b') 19
        ]
    , bgroup "search_fork"
        [ bench "1"    $ nfIO (searchFork 'a' 1)
        , bench "2"    $ nfIO (searchFork 'a' 2)
        , bench "3"    $ nfIO (searchFork 'a' 3)
        , bench "4"    $ nfIO (searchFork 'a' 4)
        , bench "5"    $ nfIO (searchFork 'a' 5)
        , bench "6"    $ nfIO (searchFork 'a' 6)
        , bench "7"    $ nfIO (searchFork 'a' 7)
        , bench "8"    $ nfIO (searchFork 'a' 8)
        , bench "9"    $ nfIO (searchFork 'a' 9)
        , bench "11"   $ nfIO (searchFork 'a' 11)
        , bench "13"   $ nfIO (searchFork 'a' 13)
        , bench "15"   $ nfIO (searchFork 'a' 15)
        , bench "17"   $ nfIO (searchFork 'a' 17)
        , bench "19"   $ nfIO (searchFork 'a' 19)
        ]
    , bgroup "search_dedicated"
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
