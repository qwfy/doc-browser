{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Match
  ( T(..)
  , defClass
  ) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Graphics.QML

-- this is what will be displayed in the search results
-- the choice of Text is due to that HsQML cannot marshal String
data T = T
  { name       :: Text
  , url        :: Text
  , collection :: Text
  , version    :: Text
  , vendor     :: Text

  -- hoogle stuff
  , package_       :: Maybe Text
  , module_        :: Maybe Text
  , typeConstraint :: Maybe Text
  } deriving (Eq, Show, Typeable, Generic, NFData)


defClass :: IO (Class T)
defClass =
  newClass
    [ defPropertyConst' "name"
        (\obj -> return (name $ fromObjRef obj))

    , defPropertyConst' "url"
        (\obj -> return (url $ fromObjRef obj))

    , defPropertyConst' "collection"
        (\obj -> return (collection $ fromObjRef obj))

    , defPropertyConst' "vendor"
        (\obj -> return (vendor $ fromObjRef obj))

    , defPropertyConst' "version"
        (\obj -> return (version $ fromObjRef obj))

    , defPropertyConst' "package_"
        (\obj -> return (package_ $ fromObjRef obj))

    , defPropertyConst' "module_"
        (\obj -> return (module_ $ fromObjRef obj))

    , defPropertyConst' "typeConstraint"
        (\obj -> return (typeConstraint $ fromObjRef obj))
    ]
