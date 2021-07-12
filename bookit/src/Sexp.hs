{-# LANGUAGE OverloadedStrings #-}

module Sexp where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import TextShow (showt)
import Prelude hiding (fail, head, length, tail)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Sexp = Sexp {unSexp :: Vector Expr}

data Expr
  = ExprAtom !Atom
  | ExprSexp !Sexp

data Atom
  = AtomSym !Symbol
  | AtomString !Str

newtype Symbol = Symbol {unSymbol :: Text} deriving (Eq)

newtype Str = Str {unStr :: Text}

-------------------------------------------------------------------------------
-- Types for reading from Sexps
-------------------------------------------------------------------------------

data ReadResult a
  = Success a
  | Failure !ReadError

instance Functor ReadResult where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

instance Applicative ReadResult where
  pure = Success
  Success f <*> Success x = Success (f x)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure g = Failure (e <> g)

instance Monad ReadResult where
  Success x >>= f = f x
  Failure e >>= _ = Failure e

newtype ReadError = ReadError {unReadError :: Text}

instance Semigroup ReadError where
  ReadError l <> ReadError r = ReadError (l <> r)

class SexpRead a where
  readSexp :: Sexp -> ReadResult a

-------------------------------------------------------------------------------
-- Read utilities
-------------------------------------------------------------------------------

ok :: a -> ReadResult a
ok = pure

fail :: Text -> ReadResult a
fail = Failure . ReadError

length :: Sexp -> Int
length = Vector.length . unSexp

head :: Sexp -> Maybe Expr
head = Vector.headM . unSexp

tail :: Sexp -> Vector Expr
tail = Vector.tail . unSexp

headAtom :: Sexp -> Maybe Atom
headAtom sexp = head sexp >>= getAtom
  where
    getAtom :: Expr -> Maybe Atom
    getAtom (ExprAtom atom) = Just atom
    getAtom _ = Nothing

headSymbol :: Sexp -> Maybe Symbol
headSymbol sexp = headAtom sexp >>= getSymbol
  where
    getSymbol :: Atom -> Maybe Symbol
    getSymbol (AtomSym symbol) = Just symbol
    getSymbol _ = Nothing

startsWithSymbol :: Symbol -> Sexp -> ReadResult ()
startsWithSymbol s sexp =
  case headSymbol sexp of
    Just symbol | s == symbol -> Success ()
    _ -> fail $ "Sexp did not start with symbol " <> unSymbol s

arity :: Sexp -> Int
arity sexp = length sexp - 1

arityMatches :: Int -> Sexp -> ReadResult ()
arityMatches n sexp =
  if arity sexp == n
    then Success ()
    else fail $ "Sexp had arity " <> showt (arity sexp) <> ", not " <> showt n

matchFn ::
  Symbol ->
  Int ->
  (Vector Expr -> ReadResult a) ->
  Sexp ->
  ReadResult a
matchFn sym arty f sexp = do
  startsWithSymbol sym sexp
  arityMatches arty sexp
  f (tail sexp)

matchFnT :: Symbol -> (Str -> ReadResult a) -> Sexp -> ReadResult a
matchFnT sym f sexp = matchFn sym 1 g sexp
  where
    g :: Vector Expr -> ReadResult a
    g vexp = undefined
