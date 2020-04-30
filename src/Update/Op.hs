--
-- Add, Replace, Delete operations on files
-- according to SrcSpan(s)
--
module Update.Op (
    Op(..)
  , toSpanUpdate
  , updateFile
  , updateFileDry
  ) where


import Update.Span (SpanUpdate(..), SrcSpan(..), SourcePos)
import Text.Megaparsec.Pos.Util

import qualified Data.Text.IO
import qualified Update.Span

data Op = Replace SrcSpan | Add SourcePos | Delete SrcSpan
  deriving (Show, Eq, Ord)

-- | Convert `Op` to `SpanUpdate`
toSpanUpdate :: Op -> SpanUpdate
toSpanUpdate (Add srcPos) = SpanUpdate {
    spanUpdateSpan = SrcSpan { spanBegin = incCol srcPos, spanEnd = incCol srcPos }
  , spanUpdateContents = mempty
  }
toSpanUpdate (Replace srcSpan) = SpanUpdate srcSpan mempty
toSpanUpdate (Delete srcSpan) = SpanUpdate srcSpan mempty

updateFile :: FilePath -> SpanUpdate -> IO ()
updateFile fp u = Data.Text.IO.readFile fp >>= Data.Text.IO.writeFile fp . Update.Span.updateSpan u

updateFileDry :: FilePath -> SpanUpdate -> IO ()
updateFileDry fp u = Data.Text.IO.readFile fp >>= Data.Text.IO.putStr . Update.Span.updateSpan u
