{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Sexp.Encode where

import Bookit.Sexp.Types
  ( Atom (AtomStr, AtomSym),
    Loc,
    Sexp (SexpAtom, SexpList),
    Str,
    Sym,
  )
import Control.Monad.State.Strict (StateT, lift)
import qualified Control.Monad.State.Strict as State
import Data.Either.Extra (mapLeft)
import Data.Set (Set)
import qualified Data.Set as Set

data Expected
  = ExpectedStr
  | ExpectedSym (Maybe Sym)
  | ExpectedAtom
  | ExpectedSexp
  | ExpectedEnd
  deriving (Eq, Ord)

data Unexpected
  = UnexpectedEnd
  | UnexpectedItem !(Sexp Loc)

data ErrLoc
  = ErrLocUnknown
  | ErrLoc !Loc

data DecodeFailed = DecodeFailed
  { unexpected :: Unexpected,
    expected :: Set Expected
  }

class Encode a where
  encodeSexp :: a -> Sexp ()

class Decode a where
  decodeSexp :: Sexp Loc -> Either DecodeFailed a

-------------------------------------------------------------------------------

type Matcher a = StateT [Sexp Loc] (Either DecodeFailed) a

runMatcher :: Matcher a -> Sexp Loc -> Either DecodeFailed a
runMatcher matcher s =
  case s of
    a@(SexpAtom _ _) ->
      Left $ DecodeFailed (UnexpectedItem a) (Set.fromList [ExpectedSexp])
    SexpList _ xs -> State.evalStateT completeMatcher xs
  where
    completeMatcher = do
      r <- matcher
      finished
      pure r

-- | Try a matcher.
try :: Matcher a -> Matcher a
try matcher = do
  s <- State.get
  let e = State.runStateT matcher s
  case e of
    Left f -> lift (Left f)
    Right (result, s') -> do
      State.put s'
      pure result

-- | Match the end of the S-expression list.
finished :: Matcher ()
finished = do
  xs <- State.get
  case xs of
    [] -> pure ()
    x : _ -> replaceExpected ExpectedEnd (errUnexpectedItem x)

replaceExpected :: Expected -> Matcher a -> Matcher a
replaceExpected e =
  State.mapStateT (mapLeft (\df -> df {expected = Set.singleton e}))

errUnexpectedItem :: Sexp Loc -> Matcher a
errUnexpectedItem s = lift $ Left $ DecodeFailed (UnexpectedItem s) Set.empty

errUnexpectedEnd :: Matcher a
errUnexpectedEnd = lift $ Left $ DecodeFailed UnexpectedEnd Set.empty

sexp :: Matcher (Sexp Loc)
sexp = do
  xs <- State.get
  case xs of
    y : ys -> do
      State.put ys
      pure y
    [] -> replaceExpected ExpectedSexp errUnexpectedEnd

str :: Matcher Str
str = try $
  replaceExpected ExpectedStr $ do
    s <- sexp
    case s of
      SexpAtom _ (AtomStr x) -> pure x
      x -> errUnexpectedItem x

sym :: Maybe Sym -> Matcher Sym
sym ms = try $
  replaceExpected (ExpectedSym ms) $ do
    s <- sexp
    case s of
      SexpAtom _ (AtomSym x) ->
        case ms of
          Nothing -> pure x
          Just es | es == x -> pure x
          _ -> errUnexpectedItem s
      _ -> errUnexpectedItem s
