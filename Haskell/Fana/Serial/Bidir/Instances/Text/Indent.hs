module Fana.Serial.Bidir.Instances.Text.Indent
(
	IndentDepth, LineWithIndent, 
	line_with_indent,
	lines_with_indent,
	TextTreeSerializer, text_tree,
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Char, String)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Tree as Base
import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Optic.Concrete.Categories.PartialIso as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Lines as Serial


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
lines_with_indent = Serial.lines >**> Optic.lift_iso line_with_indent


type TextTreeSerializer l h = Optic.PartialIso (TreeSerial.HightListParseError String) l l h h

layer_indentation :: TextTreeSerializer String [(TreeSerial.Hight, String)]
layer_indentation = convert_from_describing_class_4 lines_with_indent

text_tree :: TextTreeSerializer String [Base.Tree String]
text_tree = layer_indentation >**> TreeSerial.serializer
