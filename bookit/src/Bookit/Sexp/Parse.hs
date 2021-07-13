{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
module Bookit.Sexp.Parse where

import Bookit.Sexp.Types
  ( Atom (AtomStr, AtomSym),
    Col (Col),
    Expr (ExprAtom, ExprSexp),
    Pos (Pos),
    Row (Row),
    Sexp (Sexp),
    SrcPos (SrcPos),
    SrcRange (SrcRange),
    Str (Str),
    Sym (Sym),
  )
import Data.Char (ord)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

type Parser = MP.Parsec Void Text

getSrcPos :: Parser SrcPos
getSrcPos = do
  MP.SourcePos _ line col <- MP.getSourcePos
  let r = Row . fromIntegral . MP.unPos $ line
      c = Col . fromIntegral . MP.unPos $ col
      p = SrcPos r c
  pure p

pos :: Parser a -> Parser (Pos a)
pos p = do
  spos <- getSrcPos
  value <- p
  epos <- getSrcPos
  let range = SrcRange spos epos
      posn = Pos range value
  pure posn

sexp :: Parser Sexp
sexp = do
  _ <- MP.single '('
  _ <- MP.optional ws
  es <- pos expr `MP.sepBy` ws
  _ <- MP.optional ws
  _ <- MP.single ')'
  pure . Sexp $ es

expr :: Parser Expr
expr =
  (ExprAtom <$> atom)
    <|> (ExprSexp <$> sexp)

atom :: Parser Atom
atom =
  (AtomSym <$> sym)
    <|> (AtomStr <$> str)

sym :: Parser Sym
sym = do
  c <- MP.satisfy isAlpha
  cs <- MP.takeWhileP (Just "symbol characters") isAlphaNum
  let txt = Text.cons c cs
  pure . Sym $ txt

-- | Parse a 'Str' string with escaping characters
--
-- This can parse a regular string:
--
-- >>> MP.parse str "" "\"hello\""
-- Right (Str {unStr = "hello"})
--
-- Empty strings are valid:
--
-- >>> MP.parse str "" "\"\""
-- Right (Str {unStr = ""})
--
-- Strings with correctly-escaped characters are OK:
--
-- >>> MP.parse str "" "\"Hello\\n\\t\\\"world\\\"\""
-- Right (Str {unStr = "Hello\n\t\"world\""})
str :: Parser Str
str = do
  _ <- MP.single '"'
  ss <- MP.many (regTxt <|> escTxt)
  _ <- MP.single '"'
  pure . Str . mconcat $ ss
  where
    regTxt :: Parser Text
    regTxt = MP.takeWhile1P (Just "non-escaped string character") isRegChar

    escTxt :: Parser Text
    escTxt =
      (MP.chunk "\\\"" $> "\"")
        <|> (MP.chunk "\\n" $> "\n")
        <|> (MP.chunk "\\t" $> "\t")

    isRegChar :: Char -> Bool
    isRegChar c = (c /= '"') && (c /= '\\')

ws :: Parser Text
ws = MP.takeWhile1P (Just "whitespace") isWS

-------------------------------------------------------------------------------
-- Character Classes
-------------------------------------------------------------------------------

isWS :: Char -> Bool
isWS c =
  c == ' '
    || c == '\t'
    || c == '\n'
    || c == '\r'

isAlphaNum :: Char -> Bool
isAlphaNum c = isNum c || isAlpha c

isNum :: Char -> Bool
isNum = inCharRange '0' '9'

isAlpha :: Char -> Bool
isAlpha c = isAlphaLower c || isAlphaUpper c

isAlphaLower :: Char -> Bool
isAlphaLower = inCharRange 'a' 'z'

isAlphaUpper :: Char -> Bool
isAlphaUpper = inCharRange 'A' 'Z'

inCharRange :: Char -> Char -> Char -> Bool
inCharRange minC maxC c = (c' >= minC') && (c' <= maxC')
  where
    c', minC', maxC' :: Int
    c' = ord c
    minC' = ord minC
    maxC' = ord maxC

-- $setup
-- >>> :set -XOverloadedStrings
