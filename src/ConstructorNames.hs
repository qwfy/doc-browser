module ConstructorNames
  (
  ) where
-- {-# LANGUAGE TemplateHaskell #-}
--
-- module ConstructorNames
--   ( makeConstructorNames
--   ) where
--
-- import Language.Haskell.TH
-- import Data.Char
--
-- makeConstructorNames :: Name -> DecsQ
-- makeConstructorNames t = do
--   TyConI (DataD _ typeName _ _ constructors _) <- reify t
--   let names = map getName constructors
--   let functionName = mkName ((lowerCaseFirst $ nameBase typeName) ++ "ConstructorNames")
--   body <- [e|names|]
--   let dec = FunD functionName [Clause [] (NormalB body) []]
--   return [dec]
--   where
--     getName :: Con -> String
--     getName (NormalC name _) = show $ nameBase name
--     getName _ = error "Unsupported constructor"
--     lowerCaseFirst (h:t) = toLower h : t
