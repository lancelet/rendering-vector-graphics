-- |
module Bookit.Sexp.Types where

import Data.Text (Text)
import Data.Word (Word32)

data Pos a = Pos !SrcRange !a

instance Show a => Show (Pos a) where
  show (Pos r a) = "[" <> show a <> " " <> show r <> "]"

newtype Sexp = Sexp [Pos Expr]

instance Show Sexp where
  show (Sexp es) = "(" <> unwords (fmap show es) <> ")"

data Expr
  = ExprAtom !Atom
  | ExprSexp !Sexp

instance Show Expr where
  show (ExprAtom atom) = show atom
  show (ExprSexp sexp) = show sexp

data Atom
  = AtomSym !Sym
  | AtomStr !Str

instance Show Atom where
  show (AtomSym s) = show s
  show (AtomStr s) = show s

newtype Sym = Sym {unSym :: Text}

instance Show Sym where
  show (Sym x) = "Sym " <> show x

newtype Str = Str {unStr :: Text}

instance Show Str where
  show (Str s) = "Str " <> show s

newtype Row = Row {unRow :: Word32}
  deriving Show

newtype Col = Col {unCol :: Word32}
  deriving Show

data SrcPos = SrcPos !Row !Col

instance Show SrcPos where
  show (SrcPos (Row r) (Col c)) = show r <> ":" <> show c

data SrcRange = SrcRange !SrcPos !SrcPos

instance Show SrcRange where
  show (SrcRange s e) = "(" <> show s <> "-" <> show e <> ")"
