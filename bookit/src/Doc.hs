module Doc
  ( -- * Types
    Doc (Doc, unDoc),
    Div (DivHead, DivPara, DivMath),
    Heading (H1, H2),
    Spans (Spans, unSpans),
    Span (SpanText, SpanEmph, SpanMath),
    TexMath (TexMath, unTexMath),
  )
where

import Data.Text (Text)
import Data.Vector (Vector)

newtype Doc = Doc {unDoc :: Vector Div}

data Div
  = DivHead !Heading
  | DivPara !Spans
  | DivMath !TexMath

data Heading
  = H1 !Text
  | H2 !Text

newtype Spans = Spans {unSpans :: Vector Span}

data Span
  = SpanText !Text
  | SpanEmph !Text
  | SpanMath !TexMath

newtype TexMath = TexMath {unTexMath :: Text}
