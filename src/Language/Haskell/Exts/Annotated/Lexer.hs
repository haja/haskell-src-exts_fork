{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Lexer
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Lexer for Haskell, with some extensions.
--
-----------------------------------------------------------------------------

-- ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
-- ToDo: FloatTok should have three parts (integer part, fraction, exponent) (?)
-- ToDo: Use a lexical analyser generator (lx?)

module Language.Haskell.Exts.Annotated.Lexer (Token(..), lexer) where

import Language.Haskell.Exts.Annotated.SrcLoc
import Language.Haskell.Exts.Annotated.ParseMonad
import Language.Haskell.Exts.Annotated.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.ExtScheme

import Data.Char
import Data.Ratio
import Control.Monad (when)

-- import Debug.Trace (trace)

data Token
        = VarId String
        | QVarId (String,String)
        | IDupVarId (String)        -- duplicable implicit parameter
        | ILinVarId (String)        -- linear implicit parameter
        | ConId String
        | QConId (String,String)
        | DVarId [String]       -- to enable varid's with '-' in them
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)
        | IntTok (Integer, String)
        | FloatTok (Rational, String)
        | Character (Char, String)
        | StringTok (String, String)
        | IntTokHash (Integer, String)        -- 1#
        | WordTokHash (Integer, String)       -- 1##
        | FloatTokHash (Rational, String)     -- 1.0#
        | DoubleTokHash (Rational, String)    -- 1.0##
        | CharacterHash (Char, String)        -- c#
        | StringHash (String, String)         -- "Hello world!"#

-- Symbols

        | LeftParen
        | RightParen
        | LeftHashParen
        | RightHashParen
        | LeftCurlyBar
        | RightCurlyBar
        | SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly           -- a virtual close brace
        | LeftSquare
        | RightSquare
        | Comma
        | Underscore
        | BackQuote

-- Reserved operators

        | Dot           -- reserved for use with 'forall x . x'
        | DotDot
        | Colon
        | DoubleColon
        | Equals
        | Backslash
        | Bar
        | LeftArrow
        | RightArrow
        | At
        | Tilde
        | DoubleArrow
        | Minus
        | Exclamation
        | Star
        | LeftArrowTail         -- >-
        | RightArrowTail        -- -<
        | LeftDblArrowTail      -- >>-
        | RightDblArrowTail     -- -<<

-- Template Haskell
        | THExpQuote            -- [| or [e|
        | THPatQuote            -- [p|
        | THDecQuote            -- [d|
        | THTypQuote            -- [t|
        | THCloseQuote          -- |]
        | THIdEscape (String)   -- dollar x
        | THParenEscape         -- dollar (
        | THVarQuote            -- 'x (but without the x)
        | THTyQuote             -- ''T (but without the T)
        | THQuasiQuote (String,String)  -- [$...|...]

-- HaRP
        | RPGuardOpen       -- (|
        | RPGuardClose      -- |)
        | RPCAt             -- @:

-- Hsx
        | XCodeTagOpen      -- <%
        | XCodeTagClose     -- %>
        | XStdTagOpen       -- <
        | XStdTagClose      -- >
        | XCloseTagOpen     -- </
        | XEmptyTagClose    -- />
        | XPCDATA String
        | XRPatOpen             -- <[
        | XRPatClose            -- ]>

-- Pragmas

        | PragmaEnd                     -- #-}
--        | PragmaUnknown (String,String)   -- Any pragma not recognized
        | RULES
        | INLINE Bool
        | SPECIALISE
        | SPECIALISE_INLINE Bool
        | SOURCE
        | DEPRECATED
        | WARNING
        | SCC
        | GENERATED
        | CORE
        | UNPACK
        | OPTIONS (Maybe String,String)
        | CFILES  String
        | LANGUAGE
        | INCLUDE String
-- These are not yet implemented
--        | LINE

-- Reserved Ids

        | KW_As
        | KW_By         -- transform list comprehensions
        | KW_Case
        | KW_Class
        | KW_Data
        | KW_Default
        | KW_Deriving
        | KW_Do
        | KW_MDo
        | KW_Else
        | KW_Family     -- indexed type families
        | KW_Forall     -- universal/existential types
        | KW_Group      -- transform list comprehensions
        | KW_Hiding
        | KW_If
        | KW_Import
        | KW_In
        | KW_Infix
        | KW_InfixL
        | KW_InfixR
        | KW_Instance
        | KW_Let
        | KW_Module
        | KW_NewType
        | KW_Of
        | KW_Proc       -- arrows
        | KW_Rec        -- arrows
        | KW_Then
        | KW_Type
        | KW_Using      -- transform list comprehensions
        | KW_Where
        | KW_Qualified

                -- FFI
        | KW_Foreign
        | KW_Export
        | KW_Safe
        | KW_Unsafe
        | KW_Threadsafe
        | KW_StdCall
        | KW_CCall

        | EOF
        deriving (Eq,Show)

reserved_ops :: [(String,(Token, Maybe ExtScheme))]
reserved_ops = [
 ( "..", (DotDot,       Nothing) ),
 ( ":",  (Colon,        Nothing) ),
 ( "::", (DoubleColon,  Nothing) ),
 ( "=",  (Equals,       Nothing) ),
 ( "\\", (Backslash,    Nothing) ),
 ( "|",  (Bar,          Nothing) ),
 ( "<-", (LeftArrow,    Nothing) ),
 ( "->", (RightArrow,   Nothing) ),
 ( "@",  (At,           Nothing) ),
 ( "@:", (RPCAt,        Just (Any [RegularPatterns])) ),
 ( "~",  (Tilde,        Nothing) ),
 ( "=>", (DoubleArrow,  Nothing) ),
 ( "*",  (Star,         Just (Any [KindSignatures])) ),
 -- Arrows notation
 ( "-<",  (LeftArrowTail,       Just (Any [Arrows])) ),
 ( ">-",  (RightArrowTail,      Just (Any [Arrows])) ),
 ( "-<<", (LeftDblArrowTail,    Just (Any [Arrows])) ),
 ( ">>-", (RightDblArrowTail,   Just (Any [Arrows])) )
 ]

special_varops :: [(String,(Token, Maybe ExtScheme))]
special_varops = [
 -- the dot is only a special symbol together with forall, but can still be used as function composition
 ( ".",  (Dot,          Just (Any [ExplicitForall, ExistentialQuantification])) ),
 ( "-",  (Minus,        Nothing) ),
 ( "!",  (Exclamation,  Nothing) )
 ]

reserved_ids :: [(String,(Token, Maybe ExtScheme))]
reserved_ids = [
 ( "_",         (Underscore,    Nothing) ),
 ( "by",        (KW_By,         Just (Any [TransformListComp])) ),
 ( "case",      (KW_Case,       Nothing) ),
 ( "class",     (KW_Class,      Nothing) ),
 ( "data",      (KW_Data,       Nothing) ),
 ( "default",   (KW_Default,    Nothing) ),
 ( "deriving",  (KW_Deriving,   Nothing) ),
 ( "do",        (KW_Do,         Nothing) ),
 ( "else",      (KW_Else,       Nothing) ),
 ( "family",    (KW_Family,     Just (Any [TypeFamilies])) ),        -- indexed type families
 ( "forall",    (KW_Forall,     Just (Any [ExplicitForall, ExistentialQuantification])) ),    -- universal/existential quantification
 ( "group",     (KW_Group,      Just (Any [TransformListComp])) ),
 ( "if",        (KW_If,         Nothing) ),
 ( "import",    (KW_Import,     Nothing) ),
 ( "in",        (KW_In,         Nothing) ),
 ( "infix",     (KW_Infix,      Nothing) ),
 ( "infixl",    (KW_InfixL,     Nothing) ),
 ( "infixr",    (KW_InfixR,     Nothing) ),
 ( "instance",  (KW_Instance,   Nothing) ),
 ( "let",       (KW_Let,        Nothing) ),
 ( "mdo",       (KW_MDo,        Just (Any [RecursiveDo])) ),
 ( "module",    (KW_Module,     Nothing) ),
 ( "newtype",   (KW_NewType,    Nothing) ),
 ( "of",        (KW_Of,         Nothing) ),
 ( "proc",      (KW_Proc,       Just (Any [Arrows])) ),
 ( "rec",       (KW_Rec,        Just (Any [Arrows])) ),
 ( "then",      (KW_Then,       Nothing) ),
 ( "type",      (KW_Type,       Nothing) ),
 ( "using",     (KW_Using,      Just (Any [TransformListComp])) ),
 ( "where",     (KW_Where,      Nothing) ),

-- FFI
 ( "foreign",   (KW_Foreign,    Just (Any [ForeignFunctionInterface])) )
 ]


special_varids :: [(String,(Token, Maybe ExtScheme))]
special_varids = [
 ( "as",        (KW_As,         Nothing) ),
 ( "qualified", (KW_Qualified,  Nothing) ),
 ( "hiding",    (KW_Hiding,     Nothing) ),

-- FFI
 ( "export",     (KW_Export,        Just (Any [ForeignFunctionInterface])) ),
 ( "safe",       (KW_Safe,          Just (Any [ForeignFunctionInterface])) ),
 ( "unsafe",     (KW_Unsafe,        Just (Any [ForeignFunctionInterface])) ),
 ( "threadsafe", (KW_Threadsafe,    Just (Any [ForeignFunctionInterface])) ),
 ( "stdcall",    (KW_StdCall,       Just (Any [ForeignFunctionInterface])) ),
 ( "ccall",      (KW_CCall,         Just (Any [ForeignFunctionInterface])) )
 ]

pragmas :: [(String,Token)]
pragmas = [
 ( "rules",             RULES           ),
 ( "inline",            INLINE True     ),
 ( "noinline",          INLINE False    ),
 ( "notinline",         INLINE False    ),
 ( "specialise",        SPECIALISE      ),
 ( "specialize",        SPECIALISE      ),
 ( "source",            SOURCE          ),
 ( "deprecated",        DEPRECATED      ),
 ( "warning",           WARNING         ),
 ( "scc",               SCC             ),
 ( "generated",         GENERATED       ),
 ( "core",              CORE            ),
 ( "unpack",            UNPACK          ),
 ( "language",          LANGUAGE        ),
 ( "options",           OPTIONS undefined ), -- we'll tweak it before use - promise!
 ( "cfiles",            CFILES  undefined ), -- same here...
 ( "include",           INCLUDE undefined )  -- ...and here!
 ]

isIdent, isHSymbol :: Char -> Bool
isIdent   c = isAlpha c || isDigit c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

matchChar :: Char -> String -> Lex a ()
matchChar c msg = do
    s <- getInput
    if null s || head s /= c then fail msg else discard 1

-- The top-level lexer.
-- We need to know whether we are at the beginning of the line to decide
-- whether to insert layout tokens.

lexer :: (Loc Token -> P a) -> P a
lexer = runL topLexer

topLexer :: Lex a (Loc Token)
topLexer = do
    b <- pullCtxtFlag
    if b then -- trace (show cf ++ ": " ++ show VRightCurly) $
              setBOL >> getSrcLocL >>= \l -> return (Loc (mkSrcSpan l l) VRightCurly) -- the lex context state flags that we must do an empty {} - UGLY
     else do
        bol <- checkBOL
        (bol, ws) <- lexWhiteSpace bol
        -- take care of whitespace in PCDATA
        ec <- getExtContext
        case ec of
         -- if there was no linebreak, and we are lexing PCDATA,
         -- then we want to care about the whitespace.
         -- We don't bother to test for XmlSyntax, since we
         -- couldn't end up in ChildCtxt otherwise.
         Just ChildCtxt | not bol && ws -> getSrcLocL >>= \l -> return $ Loc (mkSrcSpan l l) $ XPCDATA " "
         _ -> do startToken
                 sl <- getSrcLocL
                 t <- if bol then lexBOL    -- >>= \t -> trace ("BOL: " ++ show t) (return t)
                             else lexToken  -- >>= \t -> trace (show t) (return t)
                 el <- getSrcLocL
                 return $ Loc (mkSrcSpan sl el) t

lexWhiteSpace :: Bool -> Lex a (Bool, Bool)
lexWhiteSpace bol = do
    s <- getInput
    case s of
        -- If we find a recognised pragma, we don't want to treat it as a comment.
        '{':'-':'#':rest | isRecognisedPragma rest -> return (bol, False)
        '{':'-':_ -> do
            loc <- getSrcLocL
            discard 2
            (bol, c) <- lexNestedComment bol ""
            loc2 <- getSrcLocL
            pushComment $ Comment True (mkSrcSpan loc loc2) (reverse c)
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)
        '-':'-':s | all (== '-') (takeWhile isHSymbol s) -> do
            loc    <- getSrcLocL
            discard 2
            dashes <- lexWhile (== '-')
            rest   <- lexWhile (/= '\n')
            s' <- getInput
            case s' of
                [] -> fail "Unterminated end-of-line comment"
                _ -> do
                    loc2 <- getSrcLocL
                    lexNewline >> pushComment (Comment False (mkSrcSpan loc loc2) $ dashes ++ rest)
                    lexWhiteSpace True
                    return (True, True)
        '\n':_ -> do
            lexNewline
            lexWhiteSpace True
            return (True, True)
        '\t':_ -> do
            lexTab
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)
        c:_ | isSpace c -> do
            discard 1
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)
        _ -> return (bol, False)

isRecognisedPragma :: String -> Bool
isRecognisedPragma str = let pragma = map toLower . takeWhile isAlphaNum . dropWhile isSpace $ str
                          in case lookup pragma pragmas of
                              Nothing -> False
                              _       -> True

lexNestedComment :: Bool -> String -> Lex a (Bool, String)
lexNestedComment bol str = do
    s <- getInput
    case s of
        '-':'}':_ -> discard 2 >> return (bol, str)
        '{':'-':_ -> do
            discard 2
            (bol, c) <- lexNestedComment bol ("-{" ++ str) -- rest of the subcomment
            lexNestedComment bol ("}-" ++ c  ) -- rest of this comment
        '\t':_    -> lexTab >> lexNestedComment bol ('\t':str)
        '\n':_    -> lexNewline >> lexNestedComment True ('\n':str)
        c:_       -> discard 1 >> lexNestedComment bol (c:str)
        []        -> fail "Unterminated nested comment"

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: Lex a Token
lexBOL = do
    pos <- getOffside
    -- trace ("Off: " ++ (show pos)) $ do
    case pos of
        LT -> do
                -- trace "layout: inserting '}'\n" $
            -- Set col to 0, indicating that we're still at the
            -- beginning of the line, in case we need a semi-colon too.
            -- Also pop the context here, so that we don't insert
            -- another close brace before the parser can pop it.
            setBOL
            popContextL "lexBOL"
            return VRightCurly
        EQ ->
            -- trace "layout: inserting ';'\n" $
            return SemiColon
        GT -> lexToken

lexToken :: Lex a Token
lexToken = do
    ec <- getExtContext
    -- we don't bother to check XmlSyntax since we couldn't
    -- have ended up in a non-Nothing context if it wasn't
    -- enabled.
    case ec of
     Just HarpCtxt     -> lexHarpToken
     Just TagCtxt      -> lexTagCtxt
     Just CloseTagCtxt -> lexCloseTagCtxt
     Just ChildCtxt    -> lexChildCtxt
     Just CodeTagCtxt  -> lexCodeTagCtxt
     _         -> lexStdToken


lexChildCtxt :: Lex a Token
lexChildCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '<':'%':_ -> do discard 2
                        pushExtContextL CodeTagCtxt
                        return XCodeTagOpen
        '<':'/':_ -> do discard 2
                        popExtContextL "lexChildCtxt"
                        pushExtContextL CloseTagCtxt
                        return XCloseTagOpen
        '<':'[':_ -> do discard 2
                        pushExtContextL HarpCtxt
                        return XRPatOpen
        '<':_     -> do discard 1
                        pushExtContextL TagCtxt
                        return XStdTagOpen
        _     -> lexPCDATA


lexPCDATA :: Lex a Token
lexPCDATA = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        [] -> return EOF
        _  -> case s of
            '\n':_ -> do
                x <- lexNewline >> lexPCDATA
                case x of
                 XPCDATA p -> return $ XPCDATA $ '\n':p
                 EOF -> return EOF
            '<':_ -> return $ XPCDATA ""
            _ -> do let pcd = takeWhile (\c -> not $ elem c "<\n") s
                        l = length pcd
                    discard l
                    x <- lexPCDATA
                    case x of
                     XPCDATA pcd' -> return $ XPCDATA $ pcd ++ pcd'
                     EOF -> return EOF


lexCodeTagCtxt :: Lex a Token
lexCodeTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '%':'>':_ -> do discard 2
                        popExtContextL "lexCodeTagContext"
                        return XCodeTagClose
        _     -> lexStdToken

lexCloseTagCtxt :: Lex a Token
lexCloseTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '>':_     -> do discard 1
                        popExtContextL "lexCloseTagCtxt"
                        return XStdTagClose
        _     -> lexStdToken

lexTagCtxt :: Lex a Token
lexTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '/':'>':_ -> do discard 2
                        popExtContextL "lexTagCtxt: Empty tag"
                        return XEmptyTagClose
        '>':_     -> do discard 1
                        popExtContextL "lexTagCtxt: Standard tag"
                        pushExtContextL ChildCtxt
                        return XStdTagClose
        _     -> lexStdToken

lexHarpToken :: Lex a Token
lexHarpToken = do
    -- if we ever end up here, then RegularPatterns must be on.
    s <- getInput
    case s of
        ']':'>':_ -> do discard 2
                        popExtContextL "lexHarpToken"
                        return XRPatClose
        _     -> lexStdToken

lexStdToken :: Lex a Token
lexStdToken = do
    s <- getInput
    exts <- getExtensionsL
    case s of
        [] -> return EOF

        '0':c:d:_ | toLower c == 'o' && isOctDigit d -> do
                        discard 2
                        (n, str) <- lexOctal
                        return (IntTok (n, '0':c:str))
                  | toLower c == 'x' && isHexDigit d -> do
                        discard 2
                        (n, str) <- lexHexadecimal
                        return (IntTok (n, '0':c:str))

        -- implicit parameters
        '?':c:_ | isLower c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ IDupVarId id

        '%':c:_ | isLower c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ ILinVarId id
        -- end implicit parameters

        -- harp
        '(':'|':c:_  | isHSymbol c -> discard 1 >> return LeftParen
        '(':'|':_ | RegularPatterns `elem` exts ->
                     do discard 2
                        return RPGuardOpen
        '|':')':_ | RegularPatterns `elem` exts ->
                     do discard 2
                        return RPGuardClose
        {- This is handled by the reserved_ops above.
        '@':':':_ | RegularPatterns `elem` exts ->
                     do discard 2
                        return RPCAt -}

        -- template haskell
        '[':'|':_ | TemplateHaskell `elem` exts -> do
                discard 2
                return $ THExpQuote

        '[':c:'|':_ | c == 'e' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return $ THExpQuote
                    | c == 'p' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THPatQuote
                    | c == 'd' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THDecQuote
                    | c == 't' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THTypQuote
        '[':'$':c:_ | isLower c && QuasiQuotes `elem` exts ->
                        discard 2 >> lexQuasiQuote

        '|':']':_ | TemplateHaskell `elem` exts -> do
                        discard 2
                        return THCloseQuote

        '$':c:_ | isLower c && TemplateHaskell `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ THIdEscape id
                | c == '(' && TemplateHaskell `elem` exts -> do
                        discard 2
                        return THParenEscape
        -- end template haskell

        -- hsx
        '<':'%':_ | XmlSyntax `elem` exts -> do
                        discard 2
                        pushExtContextL CodeTagCtxt
                        return XCodeTagOpen
        '<':c:_ | isAlpha c && XmlSyntax `elem` exts -> do
                        discard 1
                        pushExtContextL TagCtxt
                        return XStdTagOpen
        -- end hsx

        '(':'#':_ | UnboxedTuples `elem` exts -> do discard 2 >> return LeftHashParen

        '#':')':_ | UnboxedTuples `elem` exts -> do discard 2 >> return RightHashParen

        '{':'|':_ | Generics `elem` exts -> do discard 2 >> return LeftCurlyBar

        '|':'}':_ | Generics `elem` exts -> do discard 2 >> return RightCurlyBar

        -- pragmas

        '{':'-':'#':_ -> do discard 3 >> lexPragmaStart

        '#':'-':'}':_ -> do discard 3 >> return PragmaEnd

        c:_ | isDigit c -> lexDecimalOrFloat

            | isUpper c -> lexConIdOrQual ""

            | isLower c || c == '_' -> do
                    idents <- lexIdents
                    case idents of
                     [ident] -> case lookup ident (reserved_ids ++ special_varids) of
                                 Just (keyword, scheme) -> do
                                    -- check if an extension keyword is enabled
                                    if isEnabled scheme exts
                                     then flagKW keyword >> return keyword
                                     else return $ VarId ident
                                 Nothing -> return $ VarId ident
                     _ -> return $ DVarId idents

            | isHSymbol c -> do
                    sym <- lexWhile isHSymbol
                    return $ case lookup sym (reserved_ops ++ special_varops) of
                              Just (t , scheme) ->
                                -- check if an extension op is enabled
                                if isEnabled scheme exts
                                 then t
                                 else case c of
                                        ':' -> ConSym sym
                                        _   -> VarSym sym
                              Nothing -> case c of
                                          ':' -> ConSym sym
                                          _   -> VarSym sym

            | otherwise -> do
                    discard 1
                    case c of

                        -- First the special symbols
                        '(' ->  return LeftParen
                        ')' ->  return RightParen
                        ',' ->  return Comma
                        ';' ->  return SemiColon
                        '[' ->  return LeftSquare
                        ']' ->  return RightSquare
                        '`' ->  return BackQuote
                        '{' -> do
                            pushContextL NoLayout
                            return LeftCurly
                        '}' -> do
                            popContextL "lexStdToken"
                            return RightCurly

                        '\'' -> lexCharacter
                        '"' ->  lexString

                        _ ->    fail ("Illegal character \'" ++ show c ++ "\'\n")

      where lexIdents :: Lex a [String]
            lexIdents = do
                ident <- lexWhile isIdent
                s <- getInput
                exts <- getExtensionsL
                case s of
                 -- This is the only way we can get more than one ident in the list
                 -- and it requires XmlSyntax to be on.
                 '-':c:_ | XmlSyntax `elem` exts && isAlpha c -> do
                        discard 1
                        idents <- lexIdents
                        return $ ident : idents
                 '#':_ | MagicHash `elem` exts -> do
                        discard 1
                        return [ident ++ "#"]
                 _ -> return [ident]

            lexQuasiQuote :: Lex a Token
            lexQuasiQuote = do
                -- We've seen and dropped [$ already
                ident <- lexWhile isIdent
                matchChar '|' "Malformed quasi-quote quoter"
                body <- lexQQBody
                return $ THQuasiQuote (ident, body)

            lexQQBody :: Lex a String
            lexQQBody = do
                s <- getInput
                case s of
                  '\\':']':_ -> do str <- lexQQBody
                                   return (']':str)
                  '\\':'|':_ -> do str <- lexQQBody
                                   return ('|':str)
                  '|':']':_  -> discard 2 >> return ""
                  _ -> do str <- lexWhile (not . (`elem` "\\|"))
                          rest <- lexQQBody
                          return (str++rest)

lexPragmaStart :: Lex a Token
lexPragmaStart = do
    lexWhile isSpace
    pr <- lexWhile isAlphaNum
    case lookup (map toLower pr) pragmas of
     Just SPECIALISE -> do
            s <- getInput
            case dropWhile isSpace $ map toLower s of
             'i':'n':'l':'i':'n':'e':_ -> do
                      lexWhile isSpace
                      discard 6
                      return $ SPECIALISE_INLINE True
             'n':'o':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile isSpace
                        discard 8
                        return $ SPECIALISE_INLINE False
             'n':'o':'t':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile isSpace
                        discard 9
                        return $ SPECIALISE_INLINE False
             _ -> return SPECIALISE

     Just (OPTIONS _) -> do     -- see, I promised we'd mask out the 'undefined'
            s <- getInput
            case s of
             '_':_  -> do
                discard 1
                com <- lexWhile isIdent
                rest <- lexRawPragma
                return $ OPTIONS (Just com, rest)
             x:_ | isSpace x -> do
                rest <- lexRawPragma
                return $ OPTIONS (Nothing, rest)
             _ -> fail "Malformed Options pragma"
     Just (CFILES _) -> do
            rest <- lexRawPragma
            return $ CFILES rest
     Just (INCLUDE _) -> do
            rest <- lexRawPragma
            return $ INCLUDE rest
     Just p ->  return p

     _      -> fail "Internal error: Unrecognised recognised pragma"
                  -- do rawStr <- lexRawPragma
                  -- return $ PragmaUnknown (pr, rawStr) -- no support for unrecognized pragmas, treat as comment
                  -- discard 3 -- #-}
                  -- topLexer -- we just discard it as a comment for now and restart -}

lexRawPragma :: Lex a String
lexRawPragma = do
    rpr <- lexRawPragmaAux
    return $ dropWhile isSpace rpr
 where lexRawPragmaAux = do
        rpr <- lexWhile (/='#')
        s <- getInput
        case s of
         '#':'-':'}':_  -> return rpr
         _ -> do
            discard 1
            rpr' <- lexRawPragma
            return $ rpr ++ '#':rpr'

lexDecimalOrFloat :: Lex a Token
lexDecimalOrFloat = do
    ds <- lexWhile isDigit
    rest <- getInput
    exts <- getExtensionsL
    case rest of
        ('.':d:_) | isDigit d -> do
                discard 1
                frac <- lexWhile isDigit
                let num = parseInteger 10 (ds ++ frac)
                    decimals = toInteger (length frac)
                (exponent, estr) <- do
                    rest2 <- getInput
                    case rest2 of
                        'e':_ -> lexExponent
                        'E':_ -> lexExponent
                        _     -> return (0,"")
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((num%1) * 10^^(exponent - decimals), ds ++ '.':frac ++ estr)
        e:_ | toLower e == 'e' -> do
                (exponent, estr) <- lexExponent
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((parseInteger 10 ds%1) * 10^^exponent, ds ++ estr)
        '#':'#':_ | MagicHash `elem` exts -> discard 2 >> return (WordTokHash (parseInteger 10 ds, ds))
        '#':_     | MagicHash `elem` exts -> discard 1 >> return (IntTokHash  (parseInteger 10 ds, ds))
        _         ->              return (IntTok      (parseInteger 10 ds, ds))

    where
    lexExponent :: Lex a (Integer, String)
    lexExponent = do
        (e:r) <- getInput
        discard 1   -- 'e' or 'E'
        case r of
         '+':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (n, e:'+':str)
         '-':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (negate n, e:'-':str)
         d:_ | isDigit d -> lexDecimal >>= \(n,str) -> return (n, e:str)
         _ -> fail "Float with missing exponent"

lexHash :: (b -> Token) -> (b -> Token) -> Either String (b -> Token) -> Lex a (b -> Token)
lexHash a b c = do
    exts <- getExtensionsL
    if MagicHash `elem` exts
     then do
        r <- getInput
        case r of
         '#':'#':_ -> case c of
                       Right c -> discard 2 >> return c
                       Left s  -> fail s
         '#':_     -> discard 1 >> return b
         _         ->              return a
     else return a

lexConIdOrQual :: String -> Lex a Token
lexConIdOrQual qual = do
        con <- lexWhile isIdent
        let conid | null qual = ConId con
                  | otherwise = QConId (qual,con)
            qual' | null qual = con
                  | otherwise = qual ++ '.':con
        just_a_conid <- alternative (return conid)
        rest <- getInput
        exts <- getExtensionsL
        case rest of
          '.':c:_
             | isLower c || c == '_' -> do  -- qualified varid?
                    discard 1
                    ident <- lexWhile isIdent
                    s <- getInput
                    exts <- getExtensionsL
                    ident' <- case s of
                               '#':_ | MagicHash `elem` exts -> discard 1 >> return (ident ++ "#")
                               _ -> return ident
                    case lookup ident' reserved_ids of
                       -- cannot qualify a reserved word
                       Just _  -> just_a_conid
                       Nothing -> return (QVarId (qual', ident'))

             | isUpper c -> do      -- qualified conid?
                    discard 1
                    lexConIdOrQual qual'

             | isHSymbol c -> do    -- qualified symbol?
                    discard 1
                    sym <- lexWhile isHSymbol
                    exts <- getExtensionsL
                    case lookup sym reserved_ops of
                        -- cannot qualify a reserved operator
                        Just (_,scheme) | isEnabled scheme exts -> just_a_conid
                        _        -> return $ case c of
                                              ':' -> QConSym (qual', sym)
                                              _   -> QVarSym (qual', sym)

          '#':c:_
            | not (isHSymbol c) && not (isIdent c) && MagicHash `elem` exts -> do
                discard 1
                case conid of
                 ConId con -> return $ ConId $ con ++ "#"
                 QConId (q,con) -> return $ QConId (q,con ++ "#")
          _ ->  return conid -- not a qualified thing

lexCharacter :: Lex a Token
lexCharacter = do   -- We need to keep track of not only character constants but also TH 'x and ''T
        -- We've seen ' so far
        s <- getInput
        exts <- getExtensionsL
        case s of
         '\'':_ | TemplateHaskell `elem` exts -> discard 1 >> return THTyQuote
         '\\':_ -> do
                    (c,raw) <- lexEscape
                    matchQuote
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, '\\':raw))
         c:'\'':_ -> do
                    discard 2
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, [c]))
         _ | TemplateHaskell `elem` exts -> return THVarQuote
         _ -> fail "Improper character constant or misplaced \'"

    where matchQuote = matchChar '\'' "Improperly terminated character constant"


lexString :: Lex a Token
lexString = loop ("","")
    where
    loop (s,raw) = do
        r <- getInput
        exts <- getExtensionsL
        case r of
            '\\':'&':_ -> do
                    discard 2
                    loop (s, '&':'\\':raw)
            '\\':c:_ | isSpace c -> do
                        discard 1
                        wcs <- lexWhiteChars
                        matchChar '\\' "Illegal character in string gap"
                        loop (s, '\\':reverse wcs ++ '\\':raw)
                     | otherwise -> do
                        (ce, str) <- lexEscape
                        loop (ce:s, reverse str ++ '\\':raw)
            '"':'#':_ | MagicHash `elem` exts -> do
                        discard 2
                        return (StringHash (reverse s, reverse raw))
            '"':_ -> do
                discard 1
                return (StringTok (reverse s, reverse raw))
            c:_ -> do
                discard 1
                loop (c:s, c:raw)
            [] ->   fail "Improperly terminated string"

    lexWhiteChars :: Lex a String
    lexWhiteChars = do
        s <- getInput
        case s of
            '\n':_ -> do
                    lexNewline
                    wcs <- lexWhiteChars
                    return $ '\n':wcs
            '\t':_ -> do
                    lexTab
                    wcs <- lexWhiteChars
                    return $ '\t':wcs
            c:_ | isSpace c -> do
                    discard 1
                    wcs <- lexWhiteChars
                    return $ c:wcs
            _ -> return ""

lexEscape :: Lex a (Char, String)
lexEscape = do
    discard 1
    r <- getInput
    case r of

-- Production charesc from section B.2 (Note: \& is handled by caller)

        'a':_           -> discard 1 >> return ('\a', "a")
        'b':_           -> discard 1 >> return ('\b', "b")
        'f':_           -> discard 1 >> return ('\f', "f")
        'n':_           -> discard 1 >> return ('\n', "n")
        'r':_           -> discard 1 >> return ('\r', "r")
        't':_           -> discard 1 >> return ('\t', "t")
        'v':_           -> discard 1 >> return ('\v', "v")
        '\\':_          -> discard 1 >> return ('\\', "\\")
        '"':_           -> discard 1 >> return ('\"', "\"")
        '\'':_          -> discard 1 >> return ('\'', "\'")

-- Production ascii from section B.2

        '^':c:_         -> discard 2 >> cntrl c
        'N':'U':'L':_   -> discard 3 >> return ('\NUL', "NUL")
        'S':'O':'H':_   -> discard 3 >> return ('\SOH', "SOH")
        'S':'T':'X':_   -> discard 3 >> return ('\STX', "STX")
        'E':'T':'X':_   -> discard 3 >> return ('\ETX', "ETX")
        'E':'O':'T':_   -> discard 3 >> return ('\EOT', "EOT")
        'E':'N':'Q':_   -> discard 3 >> return ('\ENQ', "ENQ")
        'A':'C':'K':_   -> discard 3 >> return ('\ACK', "ACK")
        'B':'E':'L':_   -> discard 3 >> return ('\BEL', "BEL")
        'B':'S':_       -> discard 2 >> return ('\BS',  "BS")
        'H':'T':_       -> discard 2 >> return ('\HT',  "HT")
        'L':'F':_       -> discard 2 >> return ('\LF',  "LF")
        'V':'T':_       -> discard 2 >> return ('\VT',  "VT")
        'F':'F':_       -> discard 2 >> return ('\FF',  "FF")
        'C':'R':_       -> discard 2 >> return ('\CR',  "CR")
        'S':'O':_       -> discard 2 >> return ('\SO',  "SO")
        'S':'I':_       -> discard 2 >> return ('\SI',  "SI")
        'D':'L':'E':_   -> discard 3 >> return ('\DLE', "DLE")
        'D':'C':'1':_   -> discard 3 >> return ('\DC1', "DC1")
        'D':'C':'2':_   -> discard 3 >> return ('\DC2', "DC2")
        'D':'C':'3':_   -> discard 3 >> return ('\DC3', "DC3")
        'D':'C':'4':_   -> discard 3 >> return ('\DC4', "DC4")
        'N':'A':'K':_   -> discard 3 >> return ('\NAK', "NAK")
        'S':'Y':'N':_   -> discard 3 >> return ('\SYN', "SYN")
        'E':'T':'B':_   -> discard 3 >> return ('\ETB', "ETB")
        'C':'A':'N':_   -> discard 3 >> return ('\CAN', "CAN")
        'E':'M':_       -> discard 2 >> return ('\EM',  "EM")
        'S':'U':'B':_   -> discard 3 >> return ('\SUB', "SUB")
        'E':'S':'C':_   -> discard 3 >> return ('\ESC', "ESC")
        'F':'S':_       -> discard 2 >> return ('\FS',  "FS")
        'G':'S':_       -> discard 2 >> return ('\GS',  "GS")
        'R':'S':_       -> discard 2 >> return ('\RS',  "RS")
        'U':'S':_       -> discard 2 >> return ('\US',  "US")
        'S':'P':_       -> discard 2 >> return ('\SP',  "SP")
        'D':'E':'L':_   -> discard 3 >> return ('\DEL', "DEL")

-- Escaped numbers

        'o':c:_ | isOctDigit c -> do
                    discard 1
                    (n, raw) <- lexOctal
                    n <- checkChar n
                    return (n, 'o':raw)
        'x':c:_ | isHexDigit c -> do
                    discard 1
                    (n, raw) <- lexHexadecimal
                    n <- checkChar n
                    return (n, 'x':raw)
        c:_ | isDigit c -> do
                    (n, raw) <- lexDecimal
                    n <- checkChar n
                    return (n, raw)

        _       -> fail "Illegal escape sequence"

    where
    checkChar n | n <= 0x01FFFF = return (chr (fromInteger n))
    checkChar _                 = fail "Character constant out of range"

-- Production cntrl from section B.2

    cntrl :: Char -> Lex a (Char, String)
    cntrl c | c >= '@' && c <= '_' = return (chr (ord c - ord '@'), '^':c:[])
    cntrl _                        = fail "Illegal control character"

-- assumes at least one octal digit
lexOctal :: Lex a (Integer, String)
lexOctal = do
    ds <- lexWhile isOctDigit
    return (parseInteger 8 ds, ds)

-- assumes at least one hexadecimal digit
lexHexadecimal :: Lex a (Integer, String)
lexHexadecimal = do
    ds <- lexWhile isHexDigit
    return (parseInteger 16 ds, ds)

-- assumes at least one decimal digit
lexDecimal :: Lex a (Integer, String)
lexDecimal = do
    ds <- lexWhile isDigit
    return (parseInteger 10 ds, ds)

-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
    foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

flagKW :: Token -> Lex a ()
flagKW t = when (t `elem` [KW_Do, KW_MDo]) flagDo
