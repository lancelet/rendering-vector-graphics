{-# LANGUAGE OverloadedStrings #-}

module Math where

import Data.Strict.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Sexp (ReadResult, Sexp, SexpRead, readSexp)
import qualified Sexp
import Prelude hiding (Just, Maybe, Nothing)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Define = Define !Term !Alternatives

newtype Alternatives = Alternatives {unAlternatives :: Vector Alternative}

data Alternative = Alternative
  { altCond :: !(Maybe Expr),
    altExpr :: !Expr
  }

data Expr
  = ExprTerm !Term
  | ExprBin !BinOpExpr
  | ExprParen !Paren

data BinOpExpr = BinOpExpr
  { binOpExprOp :: !BinOp,
    binOpExprLeft :: !Expr,
    binOpExprRight :: !Expr
  }

newtype Paren = Paren {unParen :: Expr}

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | BEQ
  | LEQ
  | GEQ
  | BGT
  | BLT
  | Or

data Term
  = TermVec !Vec
  | TermScalar !Scalar
  | TermCoord !Coord

data Vec = Vec
  { vecName :: !Text,
    vecSubscript :: !Text
  }

data Scalar = Scalar
  { scalarName :: !Text,
    scalarSubscript :: !Text
  }

newtype Coord = Coord {unCoord :: Vector Term}

-------------------------------------------------------------------------------
-- Read from Sexp
-------------------------------------------------------------------------------

-- (scalar "p")
-- (scalar "p" "1")
instance SexpRead Scalar where
  readSexp =
    Sexp.matchFnT
      (Sexp.Symbol "scalar")
      (\(Sexp.Str name) -> Sexp.ok (Scalar name ""))

-------------------------------------------------------------------------------
-- Convert to TeX
-------------------------------------------------------------------------------

newtype TexMath = TexMath {unTexMath :: Text}

defineToTex :: Define -> TexMath
defineToTex (Define term alts) =
  if Vector.length (unAlternatives alts) == 1
    then defineSimpleToTex term (altExpr (Vector.head (unAlternatives alts)))
    else defineMultiToTex term (unAlternatives alts)

defineSimpleToTex :: Term -> Expr -> TexMath
defineSimpleToTex term expr =
  TexMath $
    unTexMath (termToTex term)
      <> " = "
      <> unTexMath (exprToTex expr)

defineMultiToTex :: Term -> Vector Alternative -> TexMath
defineMultiToTex term valt =
  TexMath $
    unTexMath (termToTex term)
      <> " = "
      <> "\\begin{cases} "
      <> Text.intercalate
        " \\ "
        (Vector.toList (Vector.map (unTexMath . altToTex) valt))
      <> " \\end{cases}"
  where
    altToTex :: Alternative -> TexMath
    altToTex (Alternative cond expr) =
      TexMath $
        unTexMath (exprToTex expr)
          <> ", & "
          <> unTexMath (condToTex cond)

    condToTex :: Maybe Expr -> TexMath
    condToTex mExpr =
      TexMath $ case mExpr of
        Nothing -> "\text{ otherwise}"
        Just expr -> "\text{ if} " <> unTexMath (exprToTex expr)

exprToTex :: Expr -> TexMath
exprToTex expr =
  case expr of
    ExprTerm term -> termToTex term
    ExprBin binOpExpr -> binOpExprToTex binOpExpr
    ExprParen paren -> parenToTex paren

binOpExprToTex :: BinOpExpr -> TexMath
binOpExprToTex (BinOpExpr op l r) =
  case op of
    Div ->
      TexMath $
        "\\frac{ "
          <> unTexMath (exprToTex l)
          <> " }{ "
          <> unTexMath (exprToTex r)
          <> " }"
    _ ->
      TexMath $
        unTexMath (exprToTex l)
          <> " "
          <> unTexMath (binOpToTex op)
          <> " "
          <> unTexMath (exprToTex r)

parenToTex :: Paren -> TexMath
parenToTex (Paren expr) =
  TexMath $ "\\left( " <> unTexMath (exprToTex expr) <> " \\right)"

binOpToTex :: BinOp -> TexMath
binOpToTex binOp = TexMath $ case binOp of
  Add -> "+"
  Sub -> "-"
  Mul -> " "
  Div -> "/"
  BEQ -> "="
  LEQ -> "\\leq"
  GEQ -> "\\geq"
  BGT -> ">"
  BLT -> "<"
  Or -> "\text{or}"

termToTex :: Term -> TexMath
termToTex term =
  case term of
    TermVec vec -> vecToTex vec
    TermScalar scalar -> scalarToTex scalar
    TermCoord coord -> coordToTex coord

vecToTex :: Vec -> TexMath
vecToTex (Vec name sub) = TexMath $ "\\mathbf{" <> name <> "}_{" <> sub <> "}"

scalarToTex :: Scalar -> TexMath
scalarToTex (Scalar name sub) = TexMath $ name <> "_{" <> sub <> "}"

coordToTex :: Coord -> TexMath
coordToTex (Coord terms) =
  TexMath $
    "\\left( "
      <> Text.intercalate
        " , "
        (Vector.toList (Vector.map (unTexMath . termToTex) terms))
      <> " \\right)"
