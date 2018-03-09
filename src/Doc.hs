module Doc where

import System.FilePath
import Data.List.Extra


data Vendor
  = DevDocs
  | Hoogle
  deriving (Show, Eq)


newtype Collection =
  Collection {getCollection :: String}
  deriving (Eq, Ord)


-- Version of the thing inside the Collection,
-- not the version of the packaging of the Collection
newtype Version =
  Version {getVersion :: String}
  deriving (Eq, Ord)


-- TODO @incomplete: handle multiple occurrences of "=="
combineCollectionVersion :: Collection -> Version -> String
combineCollectionVersion (Collection c) (Version v) =
  c ++ "==" ++ v

-- TODO @incomplete: this should be a Maybe
breakCollectionVersion :: String -> (Collection, Version)
breakCollectionVersion cv =
  let (c, v') = breakOn "==" . takeFileName $ cv
      v = drop (length "==") v'
  in (Collection c, Version v)
