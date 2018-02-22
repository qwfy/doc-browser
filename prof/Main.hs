module Main (main) where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Control.Monad.STM as STM
import Control.Concurrent

import System.Directory

import qualified Search
import qualified Devdocs

import Criterion.Main

import Data.Semigroup


main :: IO ()
main = do
  configRoot <- getXdgDirectory XdgConfig "doc-browser"
  allEntries <- Devdocs.loadAll configRoot
  allEntriesTVar <- STM.atomically $ newTVar allEntries

  let search c n =
        let txt = replicate n c
            Just query = Search.makeQuery txt
        in Search.search allEntriesTVar query 27

  let searchWithFilter n =
        let txt = replicate n 'a'
            Just query = Search.makeQuery ("/py" <> txt)
        in Search.search allEntriesTVar query 27

  terminalTm <- STM.atomically $ newEmptyTMVar

  let searchWithStartup n = do
        _ <- forkIO
               (do
                  entries <- search 'a' n
                  STM.atomically $ putTMVar terminalTm entries
               )
        -- wait for the search thread to terminate
        STM.atomically $ takeTMVar terminalTm



  defaultMain
    [ bgroup "search_alone"
        [ bench "1"    $ nfIO (search 'a' 1)
        , bench "2"    $ nfIO (search 'a' 2)
        , bench "3"    $ nfIO (search 'a' 3)
        , bench "4"    $ nfIO (search 'a' 4)
        , bench "5"    $ nfIO (search 'a' 5)
        , bench "6"    $ nfIO (search 'a' 6)
        , bench "7"    $ nfIO (search 'a' 7)
        , bench "8"    $ nfIO (search 'a' 8)
        , bench "9"    $ nfIO (search 'a' 9)
        , bench "11"   $ nfIO (search 'a' 11)
        , bench "13"   $ nfIO (search 'a' 13)
        , bench "15"   $ nfIO (search 'a' 15)
        , bench "17"   $ nfIO (search 'a' 17)
        , bench "19"   $ nfIO (search 'a' 19)
        , bench "17 b" $ nfIO (search 'b' 17)
        , bench "19 b" $ nfIO (search 'b' 19)
        ]
    , bgroup "search_with_filter"
        [ bench "1"    $ nfIO (searchWithFilter 1)
        , bench "2"    $ nfIO (searchWithFilter 2)
        , bench "3"    $ nfIO (searchWithFilter 3)
        , bench "4"    $ nfIO (searchWithFilter 4)
        , bench "5"    $ nfIO (searchWithFilter 5)
        , bench "6"    $ nfIO (searchWithFilter 6)
        , bench "7"    $ nfIO (searchWithFilter 7)
        , bench "8"    $ nfIO (searchWithFilter 8)
        , bench "9"    $ nfIO (searchWithFilter 9)
        , bench "11"   $ nfIO (searchWithFilter 11)
        , bench "13"   $ nfIO (searchWithFilter 13)
        , bench "15"   $ nfIO (searchWithFilter 15)
        , bench "17"   $ nfIO (searchWithFilter 17)
        , bench "19"   $ nfIO (searchWithFilter 19)
        ]
    , bgroup "search_with_startup"
        [ bench "1"    $ nfIO (searchWithStartup 1)
        , bench "2"    $ nfIO (searchWithStartup 2)
        , bench "3"    $ nfIO (searchWithStartup 3)
        , bench "4"    $ nfIO (searchWithStartup 4)
        , bench "5"    $ nfIO (searchWithStartup 5)
        , bench "6"    $ nfIO (searchWithStartup 6)
        , bench "7"    $ nfIO (searchWithStartup 7)
        , bench "8"    $ nfIO (searchWithStartup 8)
        , bench "9"    $ nfIO (searchWithStartup 9)
        , bench "11"   $ nfIO (searchWithStartup 11)
        , bench "13"   $ nfIO (searchWithStartup 13)
        , bench "15"   $ nfIO (searchWithStartup 15)
        , bench "17"   $ nfIO (searchWithStartup 17)
        , bench "19"   $ nfIO (searchWithStartup 19)
        ]
    ]
