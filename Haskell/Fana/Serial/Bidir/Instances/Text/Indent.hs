module Fana.Serial.Bidir.Instances.Text.Indent
(
    IndentDepth, LineWithIndent, 
    line_with_indent,
    lines_with_indent,
)
where

import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Char, String)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Lines as Serial
import qualified Fana.Serial.Bidir.Serializer as Serial


type Serializer v = Serial.Serializer () Char v
type IndentDepth = TreeSerial.Hight
type LineWithIndent = (IndentDepth, String)

indent_char :: Char
indent_char = '\t'

line_with_indent_render :: LineWithIndent -> String
line_with_indent_render (count, content) = List.replicate count indent_char <> content

line_with_indent_parse :: String -> LineWithIndent
line_with_indent_parse = List.span (== indent_char) >>> Bifunctor.first List.length >>> uncurry (,)

line_with_indent :: Optic.Iso' String LineWithIndent
line_with_indent = Optic.Iso line_with_indent_render line_with_indent_parse

lines_with_indent :: Optic.Iso' String [LineWithIndent]
lines_with_indent = Serial.lines >**> Optic.iso_up line_with_indent


