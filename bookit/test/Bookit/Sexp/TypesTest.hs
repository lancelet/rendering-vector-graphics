-- | Tests and generators for 'Bookit.Sexp.Types'.
module Bookit.Sexp.TypesTest
  ( -- * Generators
    genSexp,
    genAtomOrSexp,
    genAtom,
    genSym,
    genStr,
  )
where

import Bookit.Sexp
  ( Atom (AtomStr, AtomSym),
    Sexp (SexpAtom, SexpList),
    Str (Str),
    Sym (Sym),
  )
import Bookit.Sexp.CharTest
  ( genCharStrPlain,
    genCharSym,
    genTextStringEscapable,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-------------------------------------------------------------------------------
-- Generators for types
-------------------------------------------------------------------------------

-- | Generates parenthetical S-expressions.
genSexp :: Int -> Gen (Sexp ())
genSexp depth =
  if depth == 0
    then pure $ SexpList () []
    else SexpList () <$> Gen.list (Range.linear 1 50) (genAtomOrSexp (depth - 1))

-- | Generates atomic S-expressions or parenthetical ones.
genAtomOrSexp :: Int -> Gen (Sexp ())
genAtomOrSexp depth =
  if depth == 0
    then pure $ SexpList () []
    else
      Gen.frequency
        [ (3, SexpAtom () <$> genAtom),
          (1, genSexp (depth - 1))
        ]

genAtom :: Gen Atom
genAtom =
  Gen.choice
    [ AtomSym <$> genSym,
      AtomStr <$> genStr
    ]

genSym :: Gen Sym
genSym = Sym <$> genText (Range.linear 1 20) genCharSym

genStr :: Gen Str
genStr = Str <$> genStrText
  where
    genStrText :: Gen Text
    genStrText = mconcat <$> Gen.list (Range.linear 0 10) genTextChunk

    genTextChunk :: Gen Text
    genTextChunk =
      Gen.frequency
        [ (1, genTextStringEscapable),
          (5, genPlainChunk)
        ]

    genPlainChunk :: Gen Text
    genPlainChunk = genText (Range.linear 1 10) genCharStrPlain

genText :: Range Int -> Gen Char -> Gen Text
genText range genChar = Text.pack <$> Gen.list range genChar
