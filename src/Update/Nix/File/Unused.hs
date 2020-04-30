{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.File.Unused (normalize) where

import Nix

import qualified Data.Generics.Uniplate.Data
import qualified Data.Text

-- get rid of noise
--   bash = lowPrio (callPackage ../shells/bash/4.4.nix { });
--   XXX: not used currently, we use more generic match in `packageFile`
normalize :: NExprLoc -> NExprLoc
normalize = Data.Generics.Uniplate.Data.rewrite f
  where
    f (AnnE _ (NBinary NApp
        (AnnE _ (NSym sym))
        anyThing
      ))
      | not $ Data.Text.isSuffixOf "callPackage" sym
      = Just anyThing
      | otherwise
      = Nothing

    f _ = Nothing


