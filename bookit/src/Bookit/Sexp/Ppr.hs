{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Sexp.Ppr
  ( pprSimple,
  )
where

import Bookit.Sexp.Char as Char
import Bookit.Sexp.Types
  ( Atom (AtomStr, AtomSym),
    Sexp (SexpAtom, SexpList),
    Str,
    Sym,
    unStr,
    unSym,
  )
import Data.Text (Text)
import qualified Data.Text as Text

-- | Simple pretty-printing of S-expressions.
--
-- This prints S-expressions in a single line.
--
-- Example:
--
-- >>> let h1 = SexpAtom () (AtomSym "h1")
-- >>> let ge = SexpAtom () (AtomStr "Great Expectations")
-- >>> let example = SexpList () [ h1, ge ]
-- >>> pprSimple example
-- "(h1 \"Great Expectations\")"
pprSimple :: Sexp i -> Text
pprSimple s =
  case s of
    SexpAtom _ a -> atom a
    SexpList _ ss -> "(" <> Text.intercalate " " (fmap pprSimple ss) <> ")"

atom :: Atom -> Text
atom a =
  case a of
    AtomSym s -> sym s
    AtomStr s -> str s

sym :: Sym -> Text
sym = unSym

str :: Str -> Text
str s = "\"" <> (Char.escapeStr . unStr $ s) <> "\""

-- $setup
--
-- >>> :set -XOverloadedStrings
