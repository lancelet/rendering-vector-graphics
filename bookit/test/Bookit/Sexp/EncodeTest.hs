{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Sexp.EncodeTest where

import Bookit.Sexp (Str, Sym)
import Bookit.Sexp.Encode (Matcher)
import qualified Bookit.Sexp.Encode as Encode
import qualified Bookit.Sexp.Parse as Parse
import Data.Maybe (fromMaybe)
import Hedgehog (Group (Group), Property, property, withTests, (===))
import qualified Text.Megaparsec as MP

tests :: Group
tests =
  Group
    "Bookit.Sexp.Encode"
    [ ("test: simple Matcher example", test_matcher1)
    ]

test_matcher1 :: Property
test_matcher1 = withTests 1 $
  property $ do
    let matcher :: Matcher (Sym, Str)
        matcher = do
          sym <- Encode.sym (Just "h1")
          str <- Encode.str
          pure (sym, str)

        Just sexp = MP.parseMaybe Parse.sexp "(h1 \"hello\")"

        result :: (Sym, Str)
        Right result = Encode.runMatcher matcher sexp

    result === ("h1", "hello")
