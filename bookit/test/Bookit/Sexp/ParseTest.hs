{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Sexp.ParseTest (tests) where

import Hedgehog (property, withTests)
import qualified Hedgehog

tests :: Hedgehog.Group
tests =
  Hedgehog.Group
    "Bookit.Sexp.Parse"
    []
