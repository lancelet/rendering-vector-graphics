-- |
module Bookit.Math.Types where

import Data.Text (Text)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Define = Define
  { defineName :: !Text,
    defineSymbol :: Symbol,
    defineDescription :: !Text
  }

data Symbol
  = SymbolVector !Vector
  | SymbolScalar !Scalar

data Vector = Vector
  { vectorName :: !Text,
    vectorSubscript :: !Text
  }
  deriving (Eq, Show)

data Scalar = Scalar
  { scalarName :: !Text,
    scalarSubscript :: !Text
  }
  deriving (Eq, Show)
