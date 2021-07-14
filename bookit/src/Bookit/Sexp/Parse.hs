{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for S-expressions.
module Bookit.Sexp.Parse
  ( -- * Parsers
    sexp,
    atom,
    str,
    sym,
  )
where

import qualified Bookit.Sexp.Char as Char
import Bookit.Sexp.Types
  ( Atom (AtomStr, AtomSym),
    Loc (Loc),
    Sexp (SexpAtom, SexpList),
    SrcPos (SrcPos),
    Str (Str),
    Sym (Sym),
  )
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

-- | Parser type.
--
-- No custom error type; takes 'Text' as input.
type Parser = MP.Parsec Void Text

-- | Parse an s-expression (but not an atom).
--
-- Examples:
--
-- >>> MP.parseTest sexp "(h1)"
-- SexpList (Loc (SrcPos {row = 1, col = 1}) (SrcPos {row = 1, col = 5})) [SexpAtom (Loc (SrcPos {row = 1, col = 2}) (SrcPos {row = 1, col = 4})) (AtomSym (Sym {unSym = "h1"}))]
--
-- >>> MP.parseTest sexp "foo"
-- 1:1:
--   |
-- 1 | foo
--   | ^
-- unexpected 'f'
-- expecting s-expression: terms in parentheses
--
-- >>> MP.parseTest sexp "(h1 \"hello)"
-- 1:12:
--   |
-- 1 | (h1 "hello)
--   |            ^
-- unexpected end of input
-- expecting '"', plain string character, or string escaped character
sexp :: Parser (Sexp Loc)
sexp =
  MP.label "s-expression: terms in parentheses" $
    do
      s <- getSrcPos
      es <-
        MP.single '('
          *> optWS
          *> MP.many (sexpOrAtom <* optWS)
          <* MP.single ')'
      e <- getSrcPos
      let loc = Loc s e
      pure $ SexpList loc es

-- | Parse an s-expression or atom.
sexpOrAtom :: Parser (Sexp Loc)
sexpOrAtom =
  MP.label "atom or s-expression" $
    let sexpAtom =
          do
            s <- getSrcPos
            a <- atom
            e <- getSrcPos
            let loc = Loc s e
            pure $ SexpAtom loc a
     in sexpAtom <|> sexp

-- | Get source position.
getSrcPos :: Parser SrcPos
getSrcPos = do
  MP.SourcePos _ v h <- MP.getSourcePos
  let r = fromIntegral . MP.unPos $ v
      c = fromIntegral . MP.unPos $ h
      pos = SrcPos r c
  pure pos

-- | Parse an atom.
--
-- Examples:
--
-- >>> MP.parseTest atom "p"
-- AtomSym (Sym {unSym = "p"})
--
-- >>> MP.parseTest atom "\"hello\\nworld\""
-- AtomStr (Str {unStr = "hello\nworld"})
--
-- >>> MP.parseTest atom "@"
-- 1:1:
--   |
-- 1 | @
--   | ^
-- unexpected '@'
-- expecting atom (symbol, string)
atom :: Parser Atom
atom =
  MP.label "atom (symbol, string)" $
    (AtomSym <$> sym <|> AtomStr <$> str)

-- | Parse a string.
--
-- Examples:
--
-- >>> MP.parseTest str "\"\""
-- Str {unStr = ""}
--
-- >>> MP.parseTest str "\"string\""
-- Str {unStr = "string"}
--
-- >>> MP.parseTest str "\"We can \\\"quote\\\" content\""
-- Str {unStr = "We can \"quote\" content"}
--
-- >>> MP.parseTest str "\"Bad escape: \\@\""
-- 1:15:
--   |
-- 1 | "Bad escape: \@"
--   |               ^
-- unexpected '@'
-- expecting '"', '\', 'n', 't', or '|'
str :: Parser Str
str =
  MP.label "string" $
    Str
      <$> (MP.single '"' *> strContent <* MP.single '"')
  where
    strContent :: Parser Text
    strContent = mconcat <$> MP.many (strPlain <|> strEscaped)

    strPlain :: Parser Text
    strPlain = MP.takeWhile1P (Just "plain string character") Char.isStrPlain

    strEscaped :: Parser Text
    strEscaped =
      MP.label "string escaped character" $
        MP.single '\\'
          *> ( (MP.single 'n' $> "\n")
                 <|> (MP.single 't' $> "\t")
                 <|> (MP.single '\\' $> "\\")
                 <|> (MP.single '"' $> "\"")
                 <|> (MP.single '|' $> "|")
             )

-- | Parse a symbol.
--
-- >>> MP.parseTest sym "h1"
-- Sym {unSym = "h1"}
--
-- >>> MP.parseTest sym "+"
-- Sym {unSym = "+"}
--
-- >>> MP.parseTest sym "@"
-- 1:1:
--   |
-- 1 | @
--   | ^
-- unexpected '@'
-- expecting symbol
sym :: Parser Sym
sym = Sym <$> MP.takeWhile1P (Just "symbol") Char.isSym

-- | Parse whitespace.
ws :: Parser Text
ws = MP.takeWhile1P (Just "whitespace") Char.isWS

-- | Parse optional whitespace.
optWS :: Parser Text
optWS = MP.takeWhileP (Just "optional whitespace") Char.isWS

-- $setup
--
-- >>> :set -XOverloadedStrings
