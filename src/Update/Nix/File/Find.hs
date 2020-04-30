module Update.Nix.File.Find (
    findAttr'
  , findSet
  , findSetAttrsRec
  , findSetAttrRec
  )
  where

import Nix
import Data.Text (Text)
import Data.Generics.Uniplate.Data (para)
import Data.Maybe (catMaybes)

import Update.Nix.FetchGit.Utils (matchAttr)
import Update.Nix.File.Types
import Safe

-- Like findAttr from `Update.Nix.FetchGit.Utils` but with our `Error`
findAttr' :: Text -> [Binding a] -> Either Error (Maybe a)
findAttr' name bs = case catMaybes (matchAttr name <$> bs) of
  [x] -> pure (Just x)
  []  -> pure Nothing
  _   -> Left (DuplicateAttrs name)

-- | Extract all sets, indirection needed for some reason
-- (• Ambiguous type variable ‘f0’ arising from a use of ‘myParaHeadMay’
--    prevents the constraint ‘(Foldable f0)’ from being solved.)

findSet' :: NExprLoc -> [NT i]
findSet' = para $ \e subs ->
  case e of
  AnnE _ (NSet _rec _bindings)
    -> NN <$> pure (Just e) <*> pure []
  -- not mached, just descend
  _ -> NN Nothing <$> sequenceA subs

-- due to inderication above
findSet :: NExprLoc -> Maybe NExprLoc
findSet = headMay . concatMap catMaybesFlatten <$> findSet'

-- | Find all set attrs called `name` recursively
findSetAttrsRec :: Text -> NExprLoc -> [NExprLoc]
findSetAttrsRec name = myPara $ \e subs ->
  case e of
  AnnE _ (NSet _rec bindings)
    -> NN <$> findAttr' name bindings <*> sequenceA subs
  -- not mached, just descend
  _ -> NN Nothing <$> sequenceA subs

-- | Try to find one named set attribute recursively
findSetAttrRec :: Text -> NExprLoc -> Maybe NExprLoc
findSetAttrRec name e = headMay $ findSetAttrsRec name e
