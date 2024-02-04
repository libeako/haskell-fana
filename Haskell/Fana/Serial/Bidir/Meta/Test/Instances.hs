module Fana.Serial.Bidir.Meta.Test.Instances
(
	test,
)
where

import Prelude (Ord (..), Char, Show (..))
import Fana.Prelude.FromBase
import Fana.Develop.Test.Define (Test)
import Fana.Serial.Bidir.Instances.Text.Indent (line_with_indent)
import Fana.Serial.Bidir.Instances.Text.Delimiters (lines)
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Test

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Basic as Serial
import qualified Fana.Serial.Bidir.Instances.Concrete as Serial
import qualified Fana.Serial.Bidir.Instances.Conditioned as Serial
import qualified Fana.Serial.Bidir.Instances.Enum as Serial
import qualified Fana.Serial.Bidir.Instances.FlatSum as Serial
import qualified Fana.Serial.Bidir.Instances.Maybe as Serial
import qualified Fana.Serial.Bidir.Instances.Multiple as Serial
import qualified Fana.Serial.Bidir.Instances.ProductSum as Serial
import qualified Fana.Serial.Bidir.Instances.Tag as Serial


test_nothing :: Test
test_nothing = Test.single "nothing" (test_render_parse Serial.nothing ())

test_terminal :: Test
test_terminal = Test.single "atom" (test_serializer Serial.atom ['v'] [""])

test_maybe :: Test
test_maybe = Test.single "maybe" (test_serializer (Serial.trier Serial.atom) [Nothing, Just 'v'] [])

test_string :: Test
test_string =
	let
		the_string = "bla"
		in
			Test.single "string"
				(test_serializer (Serial.concrete_string the_string) [()] ["", "other text"])

test_conditioned :: Test
test_conditioned =
	let
		the_elem = 'c'
		in
			Test.single "conditioned"
				(test_serializer (Serial.conditioned (== the_elem) Serial.atom) [the_elem] ["", "¤"])

test_product :: Test
test_product =
	Test.single "product"
		(
			test_serializer (Serial.product (Serial.atom, Serial.atom))
			[('x', 'y')] ["", "a", "x", "y"]
		)

test_sum :: Test
test_sum =
	let
		rx = Serial.concrete_string "x"
		ry = Serial.concrete_string "y"
		in Test.single "sum" (test_serializer (Serial.sum (rx, ry)) [Left (), Right ()] ["", "a"])

test_multiple :: Test
test_multiple =
	let
		pattern = "m"
		in
			Test.single "multiple"
				(
					test_serializer
						(Serial.trier_multiple (Serial.concrete_string pattern))
						[[], [()], [(), ()]] []
				)

test_flat_sum :: Test
test_flat_sum =
	let
		serializer_terminal :: Serializer' Char Char
		serializer_terminal = Serial.tagged "char" Serial.atom
		serializer_string :: Serializer' Char ()
		serializer_string = Serial.tagged "string" (Serial.concrete_string "hello")
		case_terminal =
			let
				match :: Either Char () -> Either () Char
				match = either Right (const (Left ()))
				in Serial.FlatSumCase "atom" serializer_terminal (Optic.PartialIso Left match) False
		case_string =
			let
				match :: Either Char () -> Either () ()
				match = either (const (Left ())) Right
				in Serial.FlatSumCase "string" serializer_string (Optic.PartialIso Right match) False
		serializer_to_test = Serial.flat_sum [case_terminal, case_string]
		in
			Test.single "flat-sum"
				(test_serializer serializer_to_test [Left 'x', Right ()] ["", "bla", "char", "string"])


data Color = Red | Green | Blue
	deriving (Eq, Ord, Show)

colors :: [Color]
colors = [Red, Green, Blue]

test_enum :: Test
test_enum =
	let
		serializer_to_test =
			from_partial_iso
				(Optic.piso_convert_error (const ["unrecognised case"])
				(Serial.set show colors))
		in Test.single "enum" (test_serializer serializer_to_test [Red, Green, Blue] ["", "Redd", "Blu"])

test_whole :: Test
test_whole =
	Test.single "whole" (test_serializer (Serial.whole Serial.nothing) [()] ["c"])


--------------------------- Text

test_lines :: Test
test_lines = Test.single "lines" (Optic.test_iso_law lines ([[], ["hello"], ["hello", "world"]], []))

test_indenting :: Test
test_indenting =
	Test.single "indentation"
		(Optic.test_iso_law line_with_indent ([(0, ""), (1, "a")], []))

test_text :: Test
test_text = Test.bunch "text" [test_lines, test_indenting]


--------------------------- All

test :: Test
test =
	let
		simple_tests =
			[ test_nothing
			, test_terminal
			, test_maybe
			, test_string
			, test_product
			, test_sum
			, test_conditioned
			, test_multiple
			, test_flat_sum
			, test_enum
			, test_whole
			, test_text
			]
		in Test.bunch "combine" simple_tests
