-- |
module Bookit.Sexp.Types where

import Data.Text (Text)
import Data.Word (Word32)

data Pos a = Pos !SrcRange !a

newtype Sexp = Sexp [Pos Expr]

data Expr
  = ExprAtom !(Pos Atom)
  | ExprSexp !(Pos Sexp)

data Atom
  = AtomSym !(Pos Sym)
  | AtomStr !(Pos Str)

newtype Sym = Sym {unSym :: Text}

newtype Str = Str {unStr :: Text} deriving (Show)

newtype Row = Row {unRow :: Word32}

newtype Col = Col {unCol :: Word32}

data SrcPos = SrcPos !Row !Col

data SrcRange = SrcRange !SrcPos !SrcPos
