-- | S-expressions
module Bookit.Sexp
  ( -- * Types

    -- ** S-expressions
    Types.Sexp (..),
    Types.Atom (..),
    Types.Sym (..),
    Types.Str (..),

    -- ** Source locations
    Types.Loc (..),
    Types.SrcPos (..),

    -- * Functions

    -- ** Construction
    Types.mkLoc,

    -- ** Parsing
    parseDecode,
    Parse.parseSexp,
    Parse.sexp,

    -- ** Pretty-printing
    Ppr.pprSimple,
  )
where

import Bookit.ErrMsg (ErrMsg)
import Bookit.Sexp.Decode (Decoder)
import qualified Bookit.Sexp.Decode as Decode
import qualified Bookit.Sexp.Parse as Parse
import qualified Bookit.Sexp.Ppr as Ppr
import qualified Bookit.Sexp.Types as Types
import Data.Text (Text)

parseDecode :: Decoder a -> Text -> Either ErrMsg a
parseDecode dec txt = do
  s <- Parse.parseSexp txt
  Decode.prettyFailure txt $ Decode.runDecoder dec s
