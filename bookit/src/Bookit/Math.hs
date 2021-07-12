{-# LANGUAGE OverloadedStrings #-}

module Bookit.Math where

{-
import Data.Text (Text)
import Bookit.Sexp (Sexp, FailMsg, Matcher, Sym(Sym), Result)
import qualified Bookit.Sexp as Sexp
import Control.Applicative (optional, (<|>), some)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

data Term
  = TermVec !Vec
  | TermScalar !Scalar
  | TermCoord !Coord
  deriving (Show)

data Vec = Vec
  { vecName :: !Text,
    vecSubscript :: !Text
  }
  deriving (Show)

data Scalar = Scalar
  { scalarName :: !Text
  , scalarSubscript :: !Text
  }
  deriving (Show)

newtype Coord = Coord { unCoord :: [Term] } deriving (Show)

test :: Result Term
test = Sexp.runMatcher matchTerm (fromRight $ Sexp.textToSexp Nothing "(coord (scalar \"p\" \"x\") (scalar \"p\" \"y\"))")

test2 :: Result Scalar
test2 = Sexp.runMatcher matchScalar (fromRight $ Sexp.textToSexp Nothing "(foo \"p\")")

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight: Left!"

-------------------------------------------------------------------------------
-- Parse from Sexp
-------------------------------------------------------------------------------

matchTerm :: Matcher Term
matchTerm =
  (TermVec <$> matchVec)
  <|> (TermScalar <$> matchScalar)
  <|> (TermCoord <$> matchCoord)

matchVec :: Matcher Vec
matchVec = do
  _ <- Sexp.matchSym (Sym "vector")
  name <- Sexp.unStr <$> Sexp.str
  sub <- fromMaybe "" <$> optional (Sexp.unStr <$> Sexp.str)
  pure (Vec name sub)

matchScalar :: Matcher Scalar
matchScalar = do
  _ <- Sexp.matchSym (Sym "scalar")
  name <- Sexp.unStr <$> Sexp.str
  sub <- fromMaybe "" <$> optional (Sexp.unStr <$> Sexp.str)
  pure (Scalar name sub)

matchCoord :: Matcher Coord
matchCoord = do
  _ <- Sexp.matchSym (Sym "coord")
  terms <- some matchTerm
  pure (Coord terms)
-}
