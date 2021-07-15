{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Decoding S-expressions into types monadically.
module Bookit.Sexp.Decode where

import Bookit.Sexp.Types
  ( Atom (AtomStr, AtomSym),
    Loc,
    Sexp (SexpAtom, SexpList),
    Str,
    Sym,
  )
import Control.Applicative (Alternative, empty, liftA2, (<|>))
import Control.Monad.State.Strict (StateT (StateT))
import qualified Control.Monad.State.Strict as State
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Decoder a = Decoder {unDecoder :: StateT [Sexp Loc] Result a}

data Result a
  = Success a
  | Failure Unexpected (Set Expected)

data Unexpected
  = UnexpectedEnd
  | UnexpectedSexp !(Sexp Loc)
  deriving (Eq)

data Expected
  = ExpectedSexp
  | ExpectedStr
  | ExpectedSym !Sym
  | ExpectedAnySym
  | ExpectedEnd
  deriving (Ord, Eq)

-------------------------------------------------------------------------------

instance Functor Decoder where
  fmap f d = Decoder (f <$> unDecoder d)

instance Applicative Decoder where
  pure = Decoder . pure
  liftA2 f (Decoder x) (Decoder y) = Decoder (liftA2 f x y)

instance Alternative Decoder where
  empty = error "Decoder has no sensible empty"
  Decoder x <|> Decoder y =
    Decoder $
      StateT $ \sexpList ->
        case State.runStateT x sexpList of
          xResult@(Success _) -> xResult
          Failure u ex ->
            case State.runStateT y sexpList of
              yResult@(Success _) -> yResult
              Failure _ ey -> Failure u (ex <> ey)

instance Monad Decoder where
  decoder >>= f =
    Decoder $
      StateT $ \state ->
        case State.runStateT (unDecoder decoder) state of
          Failure u e -> Failure u e
          Success (x, state') ->
            State.runStateT (unDecoder (f x)) state'

instance Functor Result where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure u e) = Failure u e

instance Applicative Result where
  pure x = Success x
  liftA2 f (Success x) (Success y) = Success (f x y)
  liftA2 _ (Failure u e) _ = Failure u e
  liftA2 _ _ (Failure u e) = Failure u e

instance Monad Result where
  Success x >>= f = f x
  Failure u e >>= _ = Failure u e

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

runDecoder :: forall a. Decoder a -> Sexp Loc -> Result a
runDecoder decoder s =
  case s of
    a@(SexpAtom _ _) -> runDecoder (errUnexpectedSexp a) s
    SexpList _ xs -> State.evalStateT (unDecoder completeDecoder) xs
  where
    completeDecoder :: Decoder a
    completeDecoder = do
      result <- decoder
      end
      pure result

-------------------------------------------------------------------------------

sexp :: Decoder (Sexp Loc)
sexp = overrideExpected ExpectedSexp $
  Decoder $ do
    xs <- State.get
    case xs of
      [] -> unDecoder errUnexpectedEnd
      y : ys -> do
        State.put ys
        pure y

anySym :: Decoder Sym
anySym = overrideExpected ExpectedAnySym $
  Decoder $ do
    s <- unDecoder sexp
    case s of
      SexpAtom _ (AtomSym sm) -> pure sm
      x -> unDecoder $ errUnexpectedSexp x

sym :: Sym -> Decoder Sym
sym sm = overrideExpected (ExpectedSym sm) $
  Decoder $ do
    s <- unDecoder sexp
    case s of
      SexpAtom _ (AtomSym sm') | sm' == sm -> pure sm
      x -> unDecoder $ errUnexpectedSexp x

str :: Decoder Str
str = overrideExpected ExpectedStr $
  Decoder $ do
    s <- unDecoder sexp
    case s of
      SexpAtom _ (AtomStr sr) -> pure sr
      x -> unDecoder $ errUnexpectedSexp x

end :: Decoder ()
end = overrideExpected ExpectedEnd $
  Decoder $ do
    xs <- State.get
    case xs of
      [] -> pure ()
      y : _ -> unDecoder $ errUnexpectedSexp y

-------------------------------------------------------------------------------

errUnexpectedEnd :: Decoder a
errUnexpectedEnd = Decoder . StateT . const $ Failure UnexpectedEnd Set.empty

errUnexpectedSexp :: Sexp Loc -> Decoder a
errUnexpectedSexp s =
  Decoder . StateT . const $ Failure (UnexpectedSexp s) Set.empty

overrideExpected :: Expected -> Decoder a -> Decoder a
overrideExpected expected (Decoder d) =
  Decoder $
    StateT $ \sexpList ->
      case State.runStateT d sexpList of
        result@(Success _) -> result
        Failure u _ -> Failure u (Set.singleton expected)
