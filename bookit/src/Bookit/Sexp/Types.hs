{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for S-expressions.
--
-- This module contains two top-level types and a heirarchy under each:
--
--   * Types for S-expressions, under `Sexp`.
--
--   * Types for source locations of S-expressions, under `Loc`.
--
-- A typical S-expression like the following:
--
-- @
--   (h1 "Great Expectations")
-- @
--
-- could be encoded as an @'Sexp' ()@ as:
--
-- >>> let h1 = SexpAtom () (AtomSym "h1")
-- >>> let ge = SexpAtom () (AtomStr "Great Expectations")
-- >>> let example = SexpList () [ h1, ge ]
module Bookit.Sexp.Types
  ( -- * Types

    -- ** S-expressions
    Sexp (SexpAtom, SexpList),
    Atom (AtomSym, AtomStr),
    Sym (Sym, unSym),
    Str (Str, unStr),

    -- ** Source locations
    Loc (Loc),
    SrcPos (SrcPos, row, col),
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word32)

-------------------------------------------------------------------------------
-- S-expression (Symbolic Expression) Types
-------------------------------------------------------------------------------

-- | S-expression.
--
-- Type @i@ is an "index", typically used to locate an 'Sexp' within a source
-- file (ie. @'Sexp' 'Loc'@).
data Sexp i
  = -- | Atomic S-expression.
    SexpAtom i !Atom
  | -- | S-expression containing a list of 'Sexp'.
    SexpList i [Sexp i]
  deriving (Eq, Show)

-- | Atom in an S-expression.
data Atom
  = -- | Symbol atom.
    AtomSym !Sym
  | -- | String atom.
    AtomStr !Str
  deriving (Eq, Show)

-- | Symbol in an S-expression.
newtype Sym = Sym {unSym :: Text}
  deriving (Eq, Show, IsString)

-- | String in an S-expression.
newtype Str = Str {unStr :: Text}
  deriving (Eq, Show, IsString)

-------------------------------------------------------------------------------
-- Positioning of items in original source
-------------------------------------------------------------------------------

-- | Location of an item in source.
data Loc
  = Loc
      !SrcPos
      -- ^ Start position of the item.
      !SrcPos
      -- ^ End position of the item.
  deriving (Eq, Show)

-- | Source position (single row / col).
data SrcPos = SrcPos
  { -- | Row within the source.
    row :: !Word32,
    -- | Column within the source.
    col :: !Word32
  }
  deriving (Eq, Show)

-- $setup
--
-- >>> :set -XOverloadedStrings
