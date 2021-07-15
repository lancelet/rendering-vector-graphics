{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Math.Types where

import Data.Text (Text)
import qualified Data.Text as Text

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Define = Define
  { defineName :: !Text,
    defineSymbol :: Symbol,
    defineDescription :: !Text
  }
  deriving (Eq)

data Symbol
  = SymbolVector !Vector
  | SymbolScalar !Scalar
  deriving (Eq, Show)

data Vector = Vector
  { vectorName :: !Text,
    vectorSubscript :: !Text
  }
  deriving (Eq)

data Scalar = Scalar
  { scalarName :: !Text,
    scalarSubscript :: !Text
  }
  deriving (Eq)

-------------------------------------------------------------------------------

appPrec :: Int
appPrec = 10

instance Show Define where
  showsPrec d (Define n s g) =
    showParen (d > appPrec) $ \t ->
      Text.unpack
        ( Text.intercalate
            " "
            [ "Define",
              quoted n,
              Text.pack $ showsPrec (appPrec + 1) s "",
              quoted g
            ]
        )
        <> t

instance Show Vector where
  showsPrec d (Vector n s) =
    showParen (d > appPrec) $ \t ->
      Text.unpack (Text.intercalate " " ["Vector", quoted n, quoted s]) <> t

instance Show Scalar where
  showsPrec d (Scalar n s) =
    showParen (d > appPrec) $ \t ->
      Text.unpack (Text.intercalate " " ["Scalar", quoted n, quoted s]) <> t

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""
