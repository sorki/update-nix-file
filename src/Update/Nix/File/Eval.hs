{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Update.Nix.File.Eval (
    getMetaPosition
  , justdoit
  , justdoitExpr
  , justdoitWithFireworks
  , evalFindFile
  , ff
  ) where


import Data.Text (Text)

import Nix
import qualified Nix.Standard
import qualified Nix.Convert
import qualified Data.Text
import qualified Data.Time

import           Update.Nix.File.Util

import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

--import Nix.TH
import Data.String.Interpolate.IsString

-- | Get file and line of the "meta" attribute for a package
--
-- `for` can be any attrpath like `haskellPackages.zre`
getMetaPosition :: Text -> IO (FilePath, Int)
getMetaPosition for = do
  time <- Data.Time.getCurrentTime

  let Success m = parseNixTextLoc "import <nixpkgs> {}"
  let opts = (defaultOptions time) { attr = Just $ "pkgs." <> for <> ".meta.position" }

  Nix.Standard.runWithBasicEffectsIO opts $ do
    Nix.nixEvalExprLoc Nothing m >>= processResult
      (\x -> Nix.Convert.fromValue x
       >>= fromStringNoContext
       >>= return . pathPositionSplit)

-- | Strictly eval input Nix `Text` to `Text` removing effects
justdoit :: Text -> IO Text
justdoit txt = do
  time <- Data.Time.getCurrentTime

  let Success m = parseNixTextLoc txt
  let opts = (defaultOptions time)

  Nix.Standard.runWithBasicEffectsIO opts $ do
    Nix.nixEvalExprLoc Nothing m >>= processResult
      (\v -> return
        . PP.renderStrict
        . PP.layoutCompact
        . prettyNValue =<< removeEffects v)

-- | Strictly eval input Nix `NExprLoc` to `Text` removing effects
justdoitExpr :: NExprLoc -> IO Text
justdoitExpr m = do
  time <- Data.Time.getCurrentTime

  let opts = (defaultOptions time)

  Nix.Standard.runWithBasicEffectsIO opts $ do
    Nix.nixEvalExprLoc Nothing m >>= processResult
      (\v -> return
        . PP.renderStrict
        . PP.layoutCompact
        . prettyNValue =<< removeEffects v)

-- | Strictly eval input Nix `Text` to `Text` *with* effects
justdoitWithFireworks :: Text -> IO Text
justdoitWithFireworks txt = do
  time <- Data.Time.getCurrentTime

  let Success m = parseNixTextLoc txt
  let opts = (defaultOptions time)

  Nix.Standard.runWithBasicEffectsIO opts $ do
    Nix.nixEvalExprLoc Nothing m >>= processResult
      (\x -> Nix.Convert.fromValue x
       >>= fromStringNoContext
       >>= return)

-- evalFindFile "nixpkgs" -> like <nixpkgs> to `FilePath`
evalFindFile :: Text -> IO FilePath
evalFindFile = fmap Data.Text.unpack . justdoit . enclose
  where enclose x = "<" <> x <> ">"

ff :: NExprLoc
ff = let Success e = parseNixTextLoc [i| builtins.findFile [
        { path = "/etc/nixpkgs";
          prefix = "nixpkgs"; }
        ] "nixpkgs"
     |] in e
