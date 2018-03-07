{-# LANGUAGE OverloadedStrings #-}

module Match
  ( T(..)
  , defClass
  ) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Graphics.QML

-- this is what will be displayed in the search results
-- the choice of Text is due to that HsQML cannot marshal String
data T = T
  { name     :: Text
  , url      :: Text
  , language :: Text
  , version  :: Text
  } deriving (Eq, Show, Typeable)


defClass :: IO (Class Match.T)
defClass =
  newClass
    [ defPropertyConst' "name"
        (\obj -> return (Match.name $ fromObjRef obj))

    , defPropertyConst' "url"
        (\obj -> return (Match.url $ fromObjRef obj))

    , defPropertyConst' "language"
        (\obj -> return (Match.language $ fromObjRef obj))

    , defPropertyConst' "version"
        (\obj -> return (Match.version $ fromObjRef obj))
    ]
