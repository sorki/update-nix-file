{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Shorthands for Pos manipulation
--
module Text.Megaparsec.Pos.Util (
    incLine
  , incCol
  , Pos
  ) where

import Text.Megaparsec.Pos  (Pos, SourcePos(..), unPos, mkPos)
import Data.MonoTraversable (MonoFunctor(..), MonoPointed(..), Element)

type instance Element Pos = Int

instance MonoFunctor Pos where
  omap f = mkPos . f . unPos

instance MonoPointed Pos where
  opoint = mkPos

instance Num Pos where
  (+) a b = opoint $ unPos a + unPos b
  (-) a b = opoint $ unPos a - unPos b
  (*) a b = opoint $ unPos a * unPos b
  abs a = a
  signum = pure 1
  fromInteger = opoint . fromIntegral

incCol :: SourcePos -> SourcePos
incCol s = s { sourceColumn = sourceColumn s + 1 }

incLine :: SourcePos -> SourcePos
incLine s = s { sourceLine   = sourceLine   s + 1 }
