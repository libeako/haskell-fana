module Fana.Serial.Bidir.Instances.Text.Indent
(
	Count, IndentDepth, LineWithIndent, 
	line_with_indent,
	lines_with_indent,
	TextTreeSerializer, text_tree,
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Char, String, (+))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Tree as Base
import qualified Prelude as Base
import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Optic.Concrete.Categories.PartialIso as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Lines as Serial


type Count = TreeSerial.Count
type IndentDepth = TreeSerial.Hight
type LineWithIndent = (IndentDepth, String)
type ParseErrorCore = TreeSerial.HightListParseError String
type ParseErrorWithCount = TreeSerial.HightListParseError (Count, String)

indent_char :: Char
indent_char = '\t'

line_with_indent_render :: LineWithIndent -> String
line_with_indent_render (count, content) = List.replicate count indent_char <> content

line_with_indent_parse :: String -> LineWithIndent
line_with_indent_parse = List.span (== indent_char) >>> Bifunctor.first List.length >>> uncurry (,)

line_with_indent :: Optic.Iso' String LineWithIndent
line_with_indent = Optic.Iso line_with_indent_render line_with_indent_parse

lines_with_indent :: Optic.Iso String String [LineWithIndent] [(Count, LineWithIndent)]
lines_with_indent = 
	let
		without_count :: Optic.Iso' String [LineWithIndent]
		without_count = Serial.lines >**> Optic.lift_iso line_with_indent
		counts = List.iterate (+ 1) 1
		swap :: (Count, LineWithIndent) -> (IndentDepth, (Count, String))
		swap (c, (i, s)) = (i, (c, s))
		count_result :: [LineWithIndent] -> [(Count, LineWithIndent)]
		count_result = List.zip counts >>> map swap
		in Optic.change_iso_per_component id (>>> count_result) without_count

type TextTreeSerializer l1 l2 h1 h2 = Optic.PartialIso String l1 l2 h1 h2

layer_indentation :: TextTreeSerializer String String [(TreeSerial.Hight, String)] [(TreeSerial.Hight, (Count, String))]
layer_indentation = convert_from_describing_class_4 lines_with_indent

text_tree :: TextTreeSerializer String String [Base.Tree String] [Base.Tree (Count, String)]
text_tree =
	let
		error_converter :: ParseErrorWithCount -> String
		error_converter ie = "indentation error at line " <> Base.show (fst (snd (TreeSerial.hilipeFirstWrong ie)))
		in layer_indentation >**> Optic.piso_convert_error error_converter TreeSerial.serializer
