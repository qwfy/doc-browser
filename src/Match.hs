{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Match
  ( T(..)
  , defClass
  ) where

import Data.Aeson
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.List.Extra
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Graphics.QML

-- the choice of Text is due to that HsQML cannot marshal String
-- |The search result
--
-- Note: when converted to JSON, the "_" suffix in the field name will be dropped.
data T = T
  { name       :: Text -- ^The entry's main content
  , url        :: Text -- ^The full documentation of this entry can be found at this URL
  , collection :: Text -- ^See 'Doc.Collection'
  , version    :: Text -- ^See 'Doc.Version'
  , vendor     :: Text -- ^See 'Doc.Vendor'

  -- hoogle stuff
  , package_       :: Maybe Text -- ^Hoogle only. The entry's package, without package version
  , module_        :: Maybe Text -- ^Hoogle only. The entry's module
  , typeConstraint :: Maybe Text -- ^Hoogle only. The entry's type constraint
  } deriving (Eq, Show, Typeable, Generic, NFData)

instance ToJSON T where
  toJSON = genericToJSON $ defaultOptions
    {fieldLabelModifier = dropWhileEnd (== '_')}

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
