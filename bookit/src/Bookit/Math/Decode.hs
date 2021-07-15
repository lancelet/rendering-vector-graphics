{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Math.Decode where

import Bookit.Math.Types
  ( Define (Define),
    Scalar (Scalar),
    Symbol (SymbolScalar, SymbolVector),
    Vector (Vector),
  )
import Bookit.Sexp.Decode (Decoder)
import qualified Bookit.Sexp.Decode as Decode
import Bookit.Sexp.Types (Str (unStr), Sym (unSym))
import Control.Applicative (optional, (<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Decoders for producing math types from S-expressions
-------------------------------------------------------------------------------

-- | Decoder for a symbol definition.
--
-- >>> decodeTest define "(define p (vector \"p\") \"test\")"
-- Define "p" (SymbolVector (Vector "p" "")) "test"
define :: Decoder Define
define = sym "define" *> (Define <$> anySym <*> Decode.nested symbol <*> str)

-- | Decoder for a symbol.
--
-- >>> decodeTest symbol "(scalar \"p\")"
-- SymbolScalar (Scalar "p" "")
--
-- >>> decodeTest symbol "(\"vector\")"
-- 1:2:
--   |
-- 1 | ("vector")
--   |  ^
-- unexpected string "vector"
-- expecting symbol scalar, symbol vector
symbol :: Decoder Symbol
symbol =
  (SymbolVector <$> vector)
  <|> (SymbolScalar <$> scalar)

-- | Decoder for a scalar.
--
-- >>> decodeTest scalar "(scalar \"p\")"
-- Scalar "p" ""
--
-- >>> decodeTest scalar "(scalar \"p\" \"x\")"
-- Scalar "p" "x"
--
-- >>> decodeTest scalar "(foo)"
-- 1:2:
--   |
-- 1 | (foo)
--   |  ^
-- unexpected symbol foo
-- expecting symbol scalar
scalar :: Decoder Scalar
scalar = sym "scalar" *> (Scalar <$> str <*> strOrEmpty)

-- | Decoder for a vector.
--
-- >>> decodeTest vector "(vector \"p\")"
-- Vector "p" ""
--
-- >>> decodeTest vector "(vector \"p\" \"1\")"
-- Vector "p" "1"
--
-- >>> decodeTest vector "\"x\""
-- 1:1:
--   |
-- 1 | "x"
--   | ^
-- unexpected '"'
-- expecting s-expression: terms in parentheses
vector :: Decoder Vector
vector = sym "vector" *> (Vector <$> str <*> strOrEmpty)

-------------------------------------------------------------------------------
-- Decoders producing 'Text'
-------------------------------------------------------------------------------

str :: Decoder Text
str = unStr <$> Decode.str

strOrEmpty :: Decoder Text
strOrEmpty = fromMaybe "" <$> optional str

sym :: Sym -> Decoder Text
sym = fmap unSym . Decode.sym

anySym :: Decoder Text
anySym = unSym <$> Decode.anySym

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Bookit.Sexp (parseDecode)
-- >>> import Bookit.ErrMsg (ErrMsg(ErrMsg))
-- >>> import qualified Data.Text as Text
--
-- >>> :{
-- decodeTest :: (Show a) => Decoder a -> Text -> IO ()
-- decodeTest decoder txt =
--   case parseDecode decoder txt of
--     Right x -> print x
--     Left (ErrMsg err) -> putStr . Text.unpack $ err
-- :}
