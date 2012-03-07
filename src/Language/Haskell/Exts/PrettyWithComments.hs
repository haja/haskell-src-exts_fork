-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.PrettyWithComments
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, Noel Winstanley 1997-2000,
--                (c) Harald Jagenteufel 2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printer for Haskell with extensions, retaining comments.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.PrettyWithComments (
    PrettyComments
    , prettyPrintComments

    , nextComments
    , printComments
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.PrettyHelper
import Language.Haskell.Exts.Pretty

import Language.Haskell.Exts.SrcLoc

prettyPrintComments :: PrettyComments a => a -> [Comment] -> String
prettyPrintComments p cs = render $ prettyComments p cs


class PrettyComments a where
        -- | Pretty-print something in isolation.
        prettyComments :: a -> [Comment] -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrecComments :: Int -> a -> [Comment] -> Doc
        prettyComments = prettyPrecComments 0
        prettyPrecComments _ = prettyComments


instance SrcInfo l => PrettyComments (A.Module l) where
    prettyComments (A.Module pos mbHead os imp decls) cs =
                markLine pos $
                myVcat $ map pretty os ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (prettyComments h cs) x])
                    (map pretty imp ++ map pretty decls)


instance SrcInfo l => PrettyComments (A.ModuleHead l) where
    prettyComments (A.ModuleHead loc m mbWarn mbExportList) cs = mySep [
        printComments cs1,
        text "module",
        pretty m,
        maybePP pretty mbWarn,
        maybePP pretty mbExportList,
        text "where"]
        where
        (cs1, cs1') = nextComments loc cs


nextComments :: SrcInfo pos => pos -> [Comment] -> ([Comment], [Comment])
nextComments loc cs = ((filter occursBefore cs), (filter (\c -> not $ occursBefore c) cs))
    where
    occursBefore :: Comment -> Bool
    occursBefore (Comment _ comLoc _) =
        ((srcSpanStartLine comLoc) < (startLine loc))
        || ((srcSpanStartLine comLoc) == (startLine loc)
            && (srcSpanStartColumn comLoc) < (startColumn loc))

printComments :: [Comment] -> Doc
printComments [] = empty
printComments cs = mySep $ (map (\(Comment _ _ c) -> text c) cs)
