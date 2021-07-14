{-# LANGUAGE OverloadedStrings #-}

-- | Tests and generators for 'Bookit.Sexp.CharTest'.
module Bookit.Sexp.CharTest
  ( -- * Tests
    tests,

    -- * Generators
    genTextStringEscapable,
    genCharSym,
    genCharStrPlain,
    genCharAlphaNum,
    genCharDigit,
    genCharAlpha,
    genCharAlphaLower,
    genCharAlphaUpper,
    genCharWS,
  )
where

import qualified Bookit.Sexp.Char as Char
import Data.Text (Text)
import Hedgehog (Gen, Group (Group), Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen

tests :: Group
tests =
  Group
    "Bookit.Sexp.Char"
    [ ( "genCharSym        produces isSym        characters",
        prop_genCharSym
      ),
      ( "genCharStrPlain   produces isStrPlain   characters",
        prop_genCharStrPlain
      ),
      ( "genCharAlphaNum   produces isAlphaNum   characters",
        prop_genCharAlphaNum
      ),
      ( "genCharDigit      produces isDigit      characters",
        prop_genCharDigit
      ),
      ( "genCharAlpha      produces isAlpha      characters",
        prop_genCharAlpha
      ),
      ( "genCharAlphaLower produces isAlphaLower characters",
        prop_genCharAlphaLower
      ),
      ( "genCharAlphaUpper produces isAlphaUpper characters",
        prop_genCharAlphaUpper
      ),
      ( "genCharWS         produces isWS         characters",
        prop_genCharWS
      )
    ]

-------------------------------------------------------------------------------
-- Tests the check that character class generators match the classes
-------------------------------------------------------------------------------

prop_genCharSym :: Property
prop_genCharSym = checkCharClass genCharSym Char.isSym

prop_genCharStrPlain :: Property
prop_genCharStrPlain = checkCharClass genCharStrPlain Char.isStrPlain

prop_genCharAlphaNum :: Property
prop_genCharAlphaNum = checkCharClass genCharAlphaNum Char.isAlphaNum

prop_genCharDigit :: Property
prop_genCharDigit = checkCharClass genCharDigit Char.isDigit

prop_genCharAlpha :: Property
prop_genCharAlpha = checkCharClass genCharAlpha Char.isAlpha

prop_genCharAlphaLower :: Property
prop_genCharAlphaLower = checkCharClass genCharAlphaLower Char.isAlphaLower

prop_genCharAlphaUpper :: Property
prop_genCharAlphaUpper = checkCharClass genCharAlphaUpper Char.isAlphaUpper

prop_genCharWS :: Property
prop_genCharWS = checkCharClass genCharWS Char.isWS

checkCharClass ::
  -- | Generator for characters in the class.
  Gen Char ->
  -- | Character class predicate.
  (Char -> Bool) ->
  -- | Property.
  Property
checkCharClass gen p = property $ do
  c <- forAll gen
  p c === True

-------------------------------------------------------------------------------
-- Generators for character classes
-------------------------------------------------------------------------------

genTextStringEscapable :: Gen Text
genTextStringEscapable =
  Gen.element
    [ "\n",
      "\t",
      "\\",
      "\""
    ]

genCharSym :: Gen Char
genCharSym =
  Gen.frequency
    [ (4, Gen.element ['+', '-', '*', '/']),
      (62, genCharAlphaNum)
    ]

genCharStrPlain :: Gen Char
genCharStrPlain =
  Gen.filter
    (\c -> c /= '"' && c /= '\\')
    Gen.alphaNum

genCharAlphaNum :: Gen Char
genCharAlphaNum =
  Gen.frequency
    [ (10, genCharDigit),
      (52, genCharAlpha)
    ]

genCharDigit :: Gen Char
genCharDigit = Gen.element ['0' .. '9']

genCharAlpha :: Gen Char
genCharAlpha = Gen.choice [genCharAlphaLower, genCharAlphaUpper]

genCharAlphaLower :: Gen Char
genCharAlphaLower = Gen.element ['a' .. 'z']

genCharAlphaUpper :: Gen Char
genCharAlphaUpper = Gen.element ['A' .. 'Z']

genCharWS :: Gen Char
genCharWS = Gen.element [' ', '\t', '\n']
