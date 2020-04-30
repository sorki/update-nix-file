{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.File.Types (
    NT(..)
  , Error(..)
  , catMaybesFlatten
  , myPara
  , myParaHeadMay
  ) where

import           Data.Data (Data)
import           Data.Text (Text)
import           Nix.Expr  (NExprLoc)


import qualified Data.Foldable
import qualified Data.Generics.Uniplate.Data
import qualified Safe

-- NT for NixTree, recursive data type we use for paramorphisms
-- from `NExprLoc` tree when doing lookups
--
-- If query is successful we have a node `NN (Just NExprLoc) _`
-- somewhere in the tree
data NT i = NN (Maybe NExprLoc) [NT i]
  deriving (Show, Functor, Foldable, Traversable)

-- Flatten NT preserving only `NN (Just NExprLoc) _` nodes
catMaybesFlatten :: NT i -> [NExprLoc]
catMaybesFlatten n@(NN _e cs) = concatMap catMaybesFlatten cs
  ++ Data.Foldable.toList (ex n)
  where
    ex (NN (Just e) _) = Just e
    ex (NN Nothing _) = Nothing

-- | Run `para` and `catMaybesFlatten` the tree leaving only
-- `NN (Just _) _` elements
myPara :: (Foldable t, Data on)
       => (on -> [t (NT i)] -> t (NT i))
       -> on
       -> [NExprLoc]
myPara f = concatMap catMaybesFlatten
  <$> Data.Generics.Uniplate.Data.para f

-- | Like `myPara` trying to extract exactly one element
myParaHeadMay :: (Foldable t, Data on)
              => (on -> [t (NT i)] -> t (NT i))
              -> on
              -> Maybe NExprLoc
myParaHeadMay f = Safe.headMay <$> myPara f

-- Custom error/warning type
data Error =
    DuplicateAttrs Text
  | CouldNotParseInput String
  | CouldNotParse String
  | SetAttrNotFound String
  | EmptySet
  | ExpectedSet
  | ExpectedNamedVar
  | ExpectedOneItemButGotMany -- pff
  | NotFound
  deriving (Eq, Show)
