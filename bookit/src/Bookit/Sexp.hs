{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Bookit.Sexp where

{-

module Bookit.Sexp
  ( -- * Types

    -- ** Sexp types
    Sexp,
    Sym (Sym, unSym),
    Str (Str, unStr),

    -- ** Evaluation types
    FailMsg (FailMsg),
    Result (Ok, Fail),
    Matcher,

    -- * Functions

    -- ** Pretty-printing
    pprSexp,

    -- ** Decomposition
    setName,
    runMatcher,
    expr,
    atom,
    sym,
    str,
    matchSym,

    -- ** Parsing
    textToSexp,
  )
where

import Control.Monad (MonadPlus, fail)
import Control.Monad.State.Strict (StateT, lift)
import qualified Control.Monad.State.Strict as State
import Data.Char (ord)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Prelude hiding (fail)
import Control.Applicative (Alternative)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Sexp = Sexp [Expr]

instance Show Sexp where
  show s = "Sexp: " <> (Text.unpack . pprSexp $ s)

data Expr
  = ExprAtom !Atom
  | ExprSexp !Sexp
  deriving (Show)

data Atom
  = AtomSym !Sym
  | AtomStr !Str
  deriving (Show)

newtype Sym = Sym {unSym :: Text} deriving (Eq, Show)

newtype Str = Str {unStr :: Text} deriving (Show)

-------------------------------------------------------------------------------
-- Pretty-print
-------------------------------------------------------------------------------

pprSexp :: Sexp -> Text
pprSexp (Sexp es) = "(" <> Text.intercalate " " (fmap pprExpr es) <> ")"

pprExpr :: Expr -> Text
pprExpr e =
  case e of
    ExprAtom a -> pprAtom a
    ExprSexp s -> pprSexp s

pprAtom :: Atom -> Text
pprAtom a =
  case a of
    AtomSym s -> pprSym s
    AtomStr s -> pprStr s

pprSym :: Sym -> Text
pprSym = unSym

pprStr :: Str -> Text
pprStr s = "\"" <> (escapeStr . unStr $ s) <> "\""

-------------------------------------------------------------------------------
-- Evaluation and Decomposition
-------------------------------------------------------------------------------

data Attempt =
  Attempt {
    attemptName :: !Text,
    attemptFailReason :: !Text
  }

newtype MatcherNames = MatcherNames [Text] deriving (Show)

instance Semigroup MatcherNames where
  (MatcherNames ls) <> (MatcherNames rs) = MatcherNames (ls <> rs)

data Result a
  = Ok a
  | Fail [Attempt]
  deriving (Show)

instance MonadFail Result where
  fail msg = Fail (MatcherNames []) (FailMsg . Text.pack $ msg)

failTxt :: Text -> Result a
failTxt txt = Fail (MatcherNames []) (FailMsg txt)

instance Alternative Result where
  empty = Fail (MatcherNames []) (FailMsg "no results")
  Ok x <|> _ = Ok x
  Fail _ _ <|> Ok x = Ok x
  Fail nsl l <|> Fail nsr r = Fail (nsl <> nsr) (l <> r)

instance MonadPlus Result

instance Functor Result where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Fail name msg) = Fail name msg

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Fail name msg = Fail name msg
  Fail name msg <*> _ = Fail name msg

instance Monad Result where
  Ok x >>= f = f x
  Fail name msg >>= _ = Fail name msg

setName :: Text -> Matcher a -> Matcher a
setName name = State.mapStateT f
  where
    f :: Result (a, ExprStack) -> Result (a, ExprStack)
    f (Ok pair) = Ok pair
    f (Fail (MatcherNames ns) msg) = Fail (MatcherNames (name : ns)) msg

newtype ExprStack = ExprStack {unExprStack :: [Expr]}

type Matcher a = StateT ExprStack Result a

getExprList :: Matcher [Expr]
getExprList = State.gets unExprStack

putExprList :: [Expr] -> Matcher ()
putExprList es = State.put (ExprStack es)

runMatcher :: Matcher a -> Sexp -> Result a
runMatcher matcher (Sexp es) = do
  pair <- State.runStateT matcher (ExprStack es)
  if null . unExprStack . snd $ pair
    then pure . fst $ pair
    else Fail (MatcherNames []) (FailMsg "did not consume all elements")

expr :: Matcher Expr
expr = do
  es <- getExprList
  case es of
    [] -> fail "no expressions left"
    e : es' -> do
      putExprList es'
      pure e

atom :: Matcher Atom
atom = setName "atom" $ do
  e <- expr
  case e of
    ExprAtom a -> pure a
    _ -> fail "found an expression, but not an atom"

sym :: Matcher Sym
sym = setName "symbol" $ do
  a <- atom
  case a of
    AtomSym s -> pure s
    _ -> fail "found an atom, but not a symbol"

str :: Matcher Str
str = setName "string" $ do
  a <- atom
  case a of
    AtomStr s -> pure s
    _ -> fail "found an atom, but not a string"

matchSym :: Sym -> Matcher Sym
matchSym expected =
  setName (unSym expected)
  $
    do
      s <- sym
      if s == expected
        then pure s
        else lift . failTxt $
          "expected to find symbol " <> unSym expected <> " but instead found symbol " <> unSym s

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

textToSexp :: Maybe FilePath -> Text -> Either FailMsg Sexp
textToSexp mFilePath txt =
  case MP.parse parseSexp fp txt of
    Right x -> Right x
    Left peb -> Left (FailMsg . Text.pack . MP.errorBundlePretty $ peb)
  where
    fp :: String
    fp = fromMaybe "(unknown)" mFilePath

type Parser = MP.Parsec Void Text

parseSexp :: Parser Sexp
parseSexp =
  Sexp
    <$> ( parseOpenParen
            *> parseOptWS
            *> parseExpr `MP.sepBy1` parseWS
            <* parseOptWS
            <* parseCloseParen
        )

parseExpr :: Parser Expr
parseExpr =
  (ExprAtom <$> parseAtom)
    <|> (ExprSexp <$> parseSexp)

parseAtom :: Parser Atom
parseAtom =
  (AtomSym <$> parseSym)
    <|> (AtomStr <$> parseStr)

parseStr :: Parser Str
parseStr = Str <$> (parseQuote *> parseContents <* parseQuote)
  where
    parseContents :: Parser Text
    parseContents = mconcat <$> MP.many (parseRegular <|> parseEscaped)

    parseRegular :: Parser Text
    parseRegular = MP.takeWhile1P (Just "regular string character") isRegChar

    parseEscaped :: Parser Text
    parseEscaped =
      (MP.chunk "\\\"" $> "\"")
        <|> (MP.chunk "\\n" $> "\n")
        <|> (MP.chunk "\\t" $> "\t")

    isRegChar :: Char -> Bool
    isRegChar c = (c /= '"') && (c /= '\\')

escapeStr :: Text -> Text
escapeStr = mconcat . fmap escapeChar . Text.unpack

escapeChar :: Char -> Text
escapeChar c =
  case c of
    '\\' -> "\\"
    '\n' -> "\\n"
    '\t' -> "\\t"
    _ -> Text.singleton c

parseSym :: Parser Sym
parseSym =
  Sym <$> do
    c <- MP.satisfy isSymbolStartChar
    cs <- MP.takeWhileP (Just "symbol characters") isSymbolChar
    pure (Text.cons c cs)

parseQuote :: Parser Char
parseQuote = MP.single '"'

parseOpenParen :: Parser Char
parseOpenParen = MP.single '('

parseCloseParen :: Parser Char
parseCloseParen = MP.single ')'

parseOptWS :: Parser ()
parseOptWS = MP.takeWhileP (Just "optional whitespace") isWhitespace $> ()

parseWS :: Parser ()
parseWS = MP.takeWhile1P (Just "whitespace") isWhitespace $> ()

isSymbolStartChar :: Char -> Bool
isSymbolStartChar = isAlpha

isSymbolChar :: Char -> Bool
isSymbolChar c = isAlpha c || isNum c

isAlpha :: Char -> Bool
isAlpha c = isAlphaUpper c || isAlphaLower c

isNum :: Char -> Bool
isNum = isInCharRange '0' '9'

isAlphaUpper :: Char -> Bool
isAlphaUpper = isInCharRange 'A' 'Z'

isAlphaLower :: Char -> Bool
isAlphaLower = isInCharRange 'a' 'z'

isWhitespace :: Char -> Bool
isWhitespace c =
  c == ' '
    || c == '\t'
    || c == '\n'
    || c == '\r'

isInCharRange :: Char -> Char -> Char -> Bool
isInCharRange minChar maxChar c = (c' >= min') && (c' <= max')
  where
    c', min', max' :: Int
    c' = ord c
    min' = ord minChar
    max' = ord maxChar

-}
