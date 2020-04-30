{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (Text)
import Options.Applicative
import Update.Nix.File

import qualified Data.Text.IO

data Cmd = Break | Unbreak | FindPackageMeta | FindFile
  deriving (Eq, Show)

data Opt = Opt {
    optCmd     :: Cmd
  , optFile    :: Text
  , optDebug   :: Bool
  , optDryRun  :: Bool
  } deriving (Eq, Show)


parseCmd = subparser
  (
     (command "break"     (info (pure Break)           (progDesc "Break package")))
  <> (command "unbreak"   (info (pure Unbreak)         (progDesc "Unbreak package")))
  <> (command "find"      (info (pure FindPackageMeta) (progDesc "Find package")))
  <> (command "find-file" (info (pure FindFile)        (progDesc "Find file")))
  )

parse :: Parser Opt
parse = Opt
  <$> parseCmd
  <*> argument str (metavar "FILE")
  <*> switch (short 'v' <> long "debug")
  <*> switch (short 'd' <> long "dry")

opts :: ParserInfo Opt
opts = info (parse <**> helper)
  (fullDesc <> progDesc "Update .nix files" <> header "update-nix-file")

main :: IO ()
main = do
  Opt{..} <- execParser opts

  let brk = setBreakage' optDebug optDryRun

  case optCmd of
    Break -> brk True optFile
    Unbreak -> brk False optFile
    FindFile -> evalFindFile optFile >>= putStrLn
    FindPackageMeta -> getMetaPosition optFile >>= Data.Text.IO.putStrLn . pathPositionToText
