-- |
module Bookit.ErrMsg where

import Data.Text

newtype ErrMsg = ErrMsg {unErrMsg :: Text}
  deriving (Eq, Show)
