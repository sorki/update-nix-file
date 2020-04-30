{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.File.Util (
    dbgExpr
  , prettyNixText
  , fromPathLit
  , pathPositionSplit
  , pathPositionToText
  ) where

import Nix       (NExpr, NExprLoc)
import Data.Text (Text)

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Nix
import qualified Safe
import qualified Text.Pretty.Simple

prettyNixText :: NExpr -> Text
prettyNixText = Data.Text.pack . show . Nix.prettyNix

dbgExpr :: NExprLoc -> a
dbgExpr e = error
  $ Data.Text.unpack
  $ Data.Text.Lazy.toStrict
  $ Text.Pretty.Simple.pShow
  $ Nix.stripAnnotation e

-- | Extract path literal `NLiteralPath` from the expression
fromPathLit :: NExprLoc -> Maybe FilePath
fromPathLit (Nix.AnnE _ (Nix.NLiteralPath x)) = Just x
fromPathLit x = error $ show x

-- | Split "./file.nix:123" into parts
pathPositionSplit :: Text -> (FilePath, Int)
pathPositionSplit = cvt . Data.Text.breakOnEnd ":"
  where
    cvt(tfp, tline) =
      ( Data.Text.unpack tfp
      , Safe.readNote "Not a line number" $ Data.Text.unpack tline)

-- | Inverse of pathPositionSplit
pathPositionToText :: (FilePath, Int) -> Text
pathPositionToText (fp, line) =
     Data.Text.pack fp
  <> (Data.Text.pack $ show line)
