{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.SrcLoc
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines various data types representing source location
-- information, of varying degree of preciseness.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.SrcLoc where

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

-- | A single position in the source.
data SrcLoc = SrcLoc
    { srcFilename :: String
    , srcLine :: Int
    , srcColumn :: Int
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A portion of the source, spanning one or more lines and zero or more columns.
data SrcSpan = SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | Returns 'srcSpanStartLine' and 'srcSpanStartColumn' in a pair.
srcSpanStart :: SrcSpan -> (Int,Int)
srcSpanStart x = (srcSpanStartLine x, srcSpanStartColumn x)

-- | Returns 'srcSpanEndLine' and 'srcSpanEndColumn' in a pair.
srcSpanEnd :: SrcSpan -> (Int,Int)
srcSpanEnd x = (srcSpanEndLine x, srcSpanEndColumn x)


-- | Combine two locations in the source to denote a span.
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (SrcLoc fn sl sc) (SrcLoc _ el ec) = SrcSpan fn sl sc el ec

-- | Merge two source spans into a single span from the start of the first
--   to the end of the second. Assumes that the two spans relate to the
--   same source file.
mergeSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpan (SrcSpan fn sl1 sc1 el1 ec1) (SrcSpan _ sl2 sc2 el2 ec2) = 
    let (sl,sc) = min (sl1,sc1) (sl2,sc2)
        (el,ec) = max (el1,ec1) (el2,ec2)
     in SrcSpan fn sl sc el ec

-- | Test if a given span starts and ends at the same location.
isNullSpan :: SrcSpan -> Bool
isNullSpan ss = srcSpanStartLine ss == srcSpanEndLine ss &&
                    srcSpanStartColumn ss >= srcSpanEndColumn ss

-- | An entity located in the source.
data Loc a = Loc
    { loc :: SrcSpan
    , unLoc :: a
    }
  deriving (Eq,Ord,Show)


-- | A portion of the source, extended with information on the position of entities within the span.
data SrcSpanInfo = SrcSpanInfo
    { srcInfoSpan    :: SrcSpan
--    , explLayout     :: Bool
    , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Generate a 'SrcSpanInfo' with no positional information for entities.
noInfoSpan :: SrcSpan -> SrcSpanInfo
noInfoSpan ss = SrcSpanInfo ss []

-- | Generate a 'SrcSpanInfo' with the supplied positional information for entities.
infoSpan :: SrcSpan -> [SrcSpan] -> SrcSpanInfo
infoSpan = SrcSpanInfo

-- | Combine two 'SrcSpanInfo's into one that spans the combined source area of
--   the two arguments, leaving positional information blank.
combSpanInfo :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
combSpanInfo s1 s2 = SrcSpanInfo
    (mergeSrcSpan (srcInfoSpan s1) (srcInfoSpan s2))
    []

-- | Short name for 'combSpanInfo'
(<++>) :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
(<++>) = combSpanInfo

-- | Optionally combine the first argument with the second,
--   or return it unchanged if the second argument is 'Nothing'.
(<+?>) :: SrcSpanInfo -> Maybe SrcSpanInfo -> SrcSpanInfo
a <+?> b = case b of {Nothing -> a; Just b -> a <++> b}

-- | Optionally combine the second argument with the first,
--   or return it unchanged if the first argument is 'Nothing'.
(<?+>) :: Maybe SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
a <?+> b = case a of {Nothing -> b; Just a -> a <++> b}

-- | Add more positional information for entities of a span.
(<**) :: SrcSpanInfo -> [SrcSpan] -> SrcSpanInfo
ss@(SrcSpanInfo {srcInfoPoints = ps}) <** xs = ss {srcInfoPoints = ps ++ xs}

-- | Merge two 'SrcSpan's and lift them to a 'SrcInfoSpan' with
--   no positional information for entities.
(<^^>) :: SrcSpan -> SrcSpan -> SrcSpanInfo
a <^^> b = noInfoSpan (mergeSrcSpan a b)

infixl 6 <^^>
infixl 5 <++>
infixl 4 <**, <+?>, <?+>

-- | A class to work over all kinds of source location information.
class SrcInfo si where
  toSrcInfo   :: SrcLoc -> [SrcSpan] -> SrcLoc -> si
  fromSrcInfo :: SrcSpanInfo -> si
  getPointLoc :: si -> SrcLoc
  fileName    :: si -> String
  startLine   :: si -> Int
  startColumn :: si -> Int

  getPointLoc si = SrcLoc (fileName si) (startLine si) (startColumn si)

instance SrcInfo SrcLoc where
  toSrcInfo s _ _ = s
  fromSrcInfo si = SrcLoc (fileName si) (startLine si) (startColumn si)
  fileName = srcFilename
  startLine = srcLine
  startColumn = srcColumn

instance SrcInfo SrcSpan where
  toSrcInfo st _ end = mkSrcSpan st end
  fromSrcInfo = srcInfoSpan
  fileName = srcSpanFilename
  startLine = srcSpanStartLine
  startColumn = srcSpanStartColumn

instance SrcInfo SrcSpanInfo where
  toSrcInfo st pts end = SrcSpanInfo (mkSrcSpan st end) pts
  fromSrcInfo = id
  fileName = fileName . srcInfoSpan
  startLine = startLine . srcInfoSpan
  startColumn = startColumn . srcInfoSpan
