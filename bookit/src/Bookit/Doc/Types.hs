-- |
module Bookit.Doc.Types where

import Data.Text (Text)

data Div
  = DivHeading !Heading
  | DivPara [Span]

data Heading = Heading HeadingLevel !Text

data HeadingLevel = H1 | H2 | H3

data Span
  = SpanText !Text
  | SpanStyled [Style] !Text

data Style
  = Italic
  | Bold
