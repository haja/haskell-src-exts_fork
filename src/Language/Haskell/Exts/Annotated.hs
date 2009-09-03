-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts
-- Copyright   :  (c) Niklas Broberg 2004-2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- An umbrella module for the various functionality
-- of the package. Also provides some convenient
-- functionality for dealing directly with source files.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated (
    -- * Re-exported modules
      module Language.Haskell.Exts.Annotated.Syntax
    , module Language.Haskell.Exts.Annotated.Build
    , module Language.Haskell.Exts.Annotated.Parser
    , module Language.Haskell.Exts.Annotated.Pretty
    , module Language.Haskell.Exts.Annotated.Fixity
    , module Language.Haskell.Exts.Annotated.SrcLoc
    , module Language.Haskell.Exts.Annotated.ExactPrint
    , module Language.Haskell.Exts.Annotated.Comments
    , module Language.Haskell.Exts.Extension
    -- * Parsing of Haskell source files
    , parseFile
    , parseFileWithMode
    , parseFileWithExts
    , parseFileContents
    , parseFileContentsWithMode
    -- * Read extensions declared in LANGUAGE pragmas
    , readExtensions
    ) where

import Language.Haskell.Exts.Annotated.Build
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Annotated.Pretty
import Language.Haskell.Exts.Annotated.SrcLoc
import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Comments
import Language.Haskell.Exts.Annotated.ExactPrint
import Language.Haskell.Exts.Extension

import Data.List
import Language.Preprocessor.Unlit

-- | Parse a source file on disk, using the default parse mode.
parseFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFile fp = parseFileWithMode (defaultParseMode { parseFilename = fp }) fp

-- | Parse a source file on disk, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithExts exts fp = parseFileWithMode (defaultParseMode { extensions = exts, parseFilename = fp }) fp

-- | Parse a source file on disk, supplying a custom parse mode.
parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithMode p fp = readFile fp >>= (return . parseFileContentsWithMode p)

-- | Parse a source file from a string using the default parse mode.
parseFileContents :: String -> ParseResult (Module SrcSpanInfo)
parseFileContents = parseFileContentsWithMode defaultParseMode

-- | Parse a source file from a string, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileContentsWithExts :: [Extension] -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithExts exts = parseFileContentsWithMode (defaultParseMode { extensions = exts })

-- | Parse a source file from a string using a custom parse mode.
parseFileContentsWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithMode p@(ParseMode fn exts ign _) rawStr =
        let md = delit fn $ ppContents rawStr
            allExts = impliesExts $ case (ign, readExtensions md) of
                                     (False,Just es) -> exts ++ es
                                     _               -> exts
         in parseModuleWithMode (p { extensions = allExts }) md


-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe [Extension]
readExtensions str = case getTopPragmas str of
        ParseOk pgms -> Just (concatMap getExts pgms)
        _            -> Nothing
  where getExts :: OptionPragma l -> [Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident _ e) = classifyExtension e

ppContents :: String -> String
ppContents = unlines . f . lines
  where f (('#':_):rest) = rest
        f x = x

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id