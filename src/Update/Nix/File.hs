{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.File (
    getPackageDef
  , findPackage
  , mapMPackages
  , setBreakage
  , setBreakage'
  , break
  , unbreak
  , goodPositionForBroken
  , isBroken
  , findBroken
  , findMeta
  , module Update.Nix.File.Eval
  , nixpkgsFile
  ) where

import Prelude hiding (break)
import Control.Monad (when, unless)
import Control.Monad.IO.Class

import Data.Generics.Uniplate.Data (para)
import Data.Text (Text)

import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified Safe

import Nix
import Nix.Atoms (NAtom(NBool))
import Nix.Render (MonadFile)

-- qualified would be nice as well, maybe..
import           Update.Nix.FetchGit.Utils
import           Update.Nix.File.Eval
import           Update.Nix.File.Find
import           Update.Nix.File.Types
import           Update.Nix.File.Util
import           Update.Span
import           Update.Op

--import Control.Concurrent.Async     (mapConcurrently)
-- | would be cool for some stuff ^^
--import Text.Pretty.Simple

nixpkgsFile :: FilePath -> IO FilePath
nixpkgsFile f = evalFindFile $ "nixpkgs" <> "/" <> Data.Text.pack f

-- | Coere `callPackage` path argument
-- * if it's not `.nix` file but a directory append `default.nix`
coercePath :: FilePath -> FilePath
coercePath x | ".nix" `Data.List.isSuffixOf` x = x
coercePath x | otherwise = x ++ "/default.nix"

-- find package definition
findPackage' :: Text -> IO (Either Error FilePath)
findPackage' pname = do
  f <- nixpkgsFile "pkgs/top-level/all-packages.nix"
  p <- parseNixFileLoc f
  case p of
    Failure e -> pure $ Left $ CouldNotParse $ show e
    Success expr -> do
      let x = packageFile pname expr
      case x of
        Left warn -> pure $ Left warn -- error $ show warn
        Right y -> do

          -- pPrint $ catMaybesFlatten y

          case catMaybesFlatten y of
            [justOne] -> return
                $ maybe (Left NotFound) Right
                $ Safe.headMay
                $ Data.Maybe.catMaybes
                $ map fromPathLit
                $ concatMap catMaybesFlatten
                $ extractCallPackagePaths justOne

            []  -> return $ Left NotFound
            _xs -> return $ Left ExpectedOneItemButGotMany

-- | Find package and return path to its definition
findPackage :: Text -> IO (Either Error FilePath)
findPackage x = do
 mp <- findPackage' x
 case mp of
  Left e -> pure $ Left e
  Right p -> do
    f <- nixpkgsFile $ "pkgs/top-level/" ++ coercePath p
    return $ Right f

-- parseOnly with our `Error` type
parseOnly' :: MonadFile m => FilePath -> m (Either Error NExprLoc)
parseOnly' f = parseNixFileLoc f >>= \case
  Success e -> pure $ Right e
  Failure parseError -> pure $ Left (CouldNotParseInput $ show parseError)

-- | Find package definition, parse the file and return its `NExprLoc`
getPackageDef :: Text -> IO (FilePath, Either Error NExprLoc)
getPackageDef n = do
  ep <- findPackage n
  case ep of
    -- XXX
    Left _ -> error "Package not found"
    Right p -> (\x -> (p, x)) <$> parseOnly' p

-- | Find named package attribute
packageFile :: Text -> NExprLoc -> Either Error (NT ())
packageFile pname = para $ \e subs -> case e of
  AnnE _ (NSet _rec bindings)
    -> NN <$> findAttr' pname bindings <*> pure [] -- sequenceA subs

  -- not mached, just descend
  _ -> NN Nothing <$> sequenceA subs

-- | Find all "callPackage(s)" calls and extract their path arguments
extractCallPackagePaths :: NExprLoc -> [NT ()]
extractCallPackagePaths = para $ \expr subs ->
  case expr of
    (AnnE _ (NBinary NApp
        fun -- (AnnE _ (NSym sym))
        e@(AnnE _ (NLiteralPath _pkgPath))
      ))
      | let mFunName = extractFuncName fun
        in maybe (False) (\name ->
            "callPackage"  `Data.Text.isSuffixOf` name
         || "callPackages" `Data.Text.isSuffixOf` name) mFunName
      -> NN <$> pure (Just e) <*> sequenceA subs
    _ -> NN Nothing <$> sequenceA subs


-- | Map monadic function to all paths found by `extractCallPackagePaths`
-- λ: x <- mapMPackages (pure $ pure True)
-- λ: length x
-- 9784 (out of like 11k)
-- .. good enough for now, better requires eval

mapMPackages :: (MonadIO m, MonadFile m) => (FilePath -> m b) -> m [b]
mapMPackages fn = do
  f <- liftIO $ nixpkgsFile "pkgs/top-level/all-packages.nix"
  p <- parseNixFileLoc f
  case p of
    Failure err -> error $ show err
    Success expr -> do
      let x = extractCallPackagePaths expr
      mapM fn
        $ Data.Maybe.catMaybes
        $ map fromPathLit
        $ concatMap catMaybesFlatten x

-- | Find "meta" attribute recursively
findMeta :: NExprLoc -> Maybe NExprLoc
findMeta = findSetAttrRec "meta"

-- | Find "broken" attribute recursively
findBroken :: NExprLoc -> Maybe NExprLoc
findBroken = findSetAttrRec "broken"

isBroken :: Maybe NExprLoc -> Maybe Bool
isBroken (Just (AnnE _ (NConstant (NBool b)))) = Just b
--- XXX: eval??
isBroken _ = Nothing -- can be complex expression

-- | Try to find good position for "meta.broken = .." attr
goodPositionForBroken :: NExprLoc -> Either Error Op
goodPositionForBroken e = case findBroken e of
  Nothing -> case findMeta e of
    Nothing -> Left $ SetAttrNotFound "meta"
    Just m -> do
      case findSet m of
        Nothing -> Left $ SetAttrNotFound "meta"
        Just s -> Add <$> lastInSet s
  x@(Just (AnnE pos _b)) | Data.Maybe.isJust (isBroken x) -> Right $ Replace pos
  -- XXX: Replace with ViewPattern ^^^^^^^^^
  _ -> Left $ SetAttrNotFound "meta.broken"

-- | Get a `SourcePos` of the last element of the set
lastInSet :: NExprLoc -> Either Error SourcePos
lastInSet (AnnE _ (NSet _rec [])) = Left EmptySet
lastInSet (AnnE _ (NSet _rec xs)) = case Safe.lastMay xs of
  Just (NamedVar _ (AnnE p _) _nvpos) -> Right $ spanEnd p
  _ -> Left ExpectedNamedVar
lastInSet _x = Left ExpectedSet

-- | Find package, parse its expression file,
-- try to find "meta" and "meta.broken" and adjust
-- iff needed
setBreakage :: Bool -> Text -> IO ()
setBreakage = setBreakage' False False

break :: Text -> IO ()
break = setBreakage True

unbreak :: Text -> IO ()
unbreak = setBreakage False

-- | Principled version for debugging and dry runs
--
-- > setBreakage' True True False "slimrat"
-- for debug dry-run ^
setBreakage' :: Bool -> Bool -> Bool -> Text -> IO ()
setBreakage' debug dryRun desiredState pkg = do
  (fp, def) <- getPackageDef pkg

  --pPrint $ findMeta <$> def
  -- XXX: much nested..

  case def of
    Left err -> error $ show err
    Right expr -> do

      let currentState = isBroken $ findBroken expr

      when (currentState == Just desiredState) $ do
        putStrLn "Package already in desired state"

      unless (currentState == Just desiredState ||
             (Data.Maybe.isNothing currentState && desiredState == False)) $ do

        let eop = goodPositionForBroken expr
        case eop of
          Left e -> error $ show e
          Right op -> do
            f <- Data.Text.IO.readFile fp

            -- XXX: replace false needs turning to delete
            -- as we don't want broken = false;
            -- maybe add forceFlag as well
            -- ! also there could be comments following the attr = value; # why it is broken
            -- (should drop whole line | scan surroundings)
            let su = toSpanUpdate op
                (_prev, l) = split (SourcePos "" (sourceLine $ spanBegin $ spanUpdateSpan su) 1) f
                addIndent = Data.Text.takeWhile (==' ') l
                u = su { spanUpdateContents = case op of
                         (Add _)     ->
                               "\n"
                            <> addIndent
                            <> "broken = "
                            <> prettyNixText (mkBool desiredState)
                            <> ";"

                         (Replace _) -> prettyNixText $ mkBool desiredState
                         (Delete _)  -> mempty
                       }

            when debug $ do
              print op
              print u

            if dryRun then updateFileDry fp u
                      else updateFile fp u
