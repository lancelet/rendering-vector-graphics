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

define :: Decoder Define
define = sym "define" *> (Define <$> anySym <*> symbol <*> str)

symbol :: Decoder Symbol
symbol = (SymbolVector <$> vector) <|> (SymbolScalar <$> scalar)

scalar :: Decoder Scalar
scalar = sym "scalar" *> (Scalar <$> str <*> strOrEmpty)

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
