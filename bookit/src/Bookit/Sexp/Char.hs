{-# LANGUAGE OverloadedStrings #-}

-- | Operations related to character classes in S-expressions.
module Bookit.Sexp.Char
  ( -- * Functions

    -- ** Escaping
    escapeStr,
    stringEscapedChar,

    -- ** Character classes
    isSym,
    isStrPlain,
    isAlphaNum,
    isDigit,
    isAlpha,
    isAlphaUpper,
    isAlphaLower,
    inCharRange,
    isWS,
  )
where

import Control.Exception (assert)
import Data.Char (ord)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Escape a string, making it suitable to be printed.
--
-- Examples:
--
-- >>> escapeStr "Result:\n\tx = \\p"
-- "Result:\\n\\tx = \\\\p"
--
-- >>> escapeStr "no escaping needed"
-- "no escaping needed"
--
-- >>> escapeStr ""
-- ""
escapeStr :: Text -> Text
escapeStr = go []
  where
    go :: [Text] -> Text -> Text
    go xs t | Text.null t = mconcat (reverse xs)
    go xs t =
      let l, r :: Text
          (l, r) = Text.break (isJust . stringEscapedChar) t
       in case Text.uncons r of
            Nothing -> go (l : xs) Text.empty
            Just (hd, tl) -> go (fromJust (stringEscapedChar hd) : l : xs) tl
{-# INLINEABLE escapeStr #-}

-- | If a character should be escaped in a string, return its escaped form.
stringEscapedChar :: Char -> Maybe Text
stringEscapedChar c =
  case c of
    '\n' -> Just "\\n"
    '\t' -> Just "\\t"
    '\\' -> Just "\\\\"
    '"' -> Just "\\\""
    '|' -> Just "\\|"
    _ -> Nothing
{-# INLINEABLE stringEscapedChar #-}

-- | Check if a character can be the start of an S-expression symbol.
isSym :: Char -> Bool
isSym c =
  let extras :: Set Char
      extras = Set.fromList ['+', '-', '*', '/']
   in isAlphaNum c || Set.member c extras
{-# INLINEABLE isSym #-}

-- | Check if a character is a plain string character.
isStrPlain :: Char -> Bool
isStrPlain c = (c /= '"') && (c /= '\\')
{-# INLINEABLE isStrPlain #-}

-- | Check if a character is an alphanumeric character.
isAlphaNum :: Char -> Bool
isAlphaNum c = isDigit c || isAlpha c
{-# INLINEABLE isAlphaNum #-}

-- | Check if a character is a decimal digit.
isDigit :: Char -> Bool
isDigit = inCharRange '0' '9'
{-# INLINEABLE isDigit #-}

-- | Check if a character is alphabetic (upper or lowercase).
isAlpha :: Char -> Bool
isAlpha c = isAlphaLower c || isAlphaUpper c
{-# INLINEABLE isAlpha #-}

-- | Check if a character is alphabetic lowercase.
isAlphaLower :: Char -> Bool
isAlphaLower = inCharRange 'a' 'z'
{-# INLINEABLE isAlphaLower #-}

-- | Check if a character is alphabetic uppercase.
isAlphaUpper :: Char -> Bool
isAlphaUpper = inCharRange 'A' 'Z'
{-# INLINEABLE isAlphaUpper #-}

-- | Check if a character is in a contiguous range.
--
-- Example:
--
-- >>> inCharRange '0' '9' '4'
-- True
-- >>> inCharRange '0' '9' 'a'
-- False
inCharRange ::
  -- | Minimum character in the range.
  Char ->
  -- | Maximum character in the range.
  Char ->
  -- | Character to test.
  Char ->
  -- | True if the character is in the range.
  Bool
inCharRange minC maxC c =
  let x, minI, maxI :: Int
      x = ord c
      minI = ord minC
      maxI = ord maxC
   in assert (minI <= maxI) $ (x >= minI) && (x <= maxI)
{-# INLINEABLE inCharRange #-}

-- | Check if a character is whitespace.
isWS :: Char -> Bool
isWS c = (c == ' ') || (c == '\t') || (c == '\n')
{-# INLINEABLE isWS #-}

-- $setup
--
-- >>> :set -XOverloadedStrings
