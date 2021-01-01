module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Lines
(
	serializer_line,
	test,
)
where

import Fana.Prelude
import Fana.Develop.Test.Define (Test)
import Prelude (Char, String)

import qualified Data.Char as Char
import qualified Data.Either as Base
import qualified Data.Maybe as Base
import qualified Fana.Convert as Fana
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Instances.Basic as Lang
import qualified Fana.Serial.Bidir.Instances.Concrete as Lang
import qualified Fana.Serial.Bidir.Instances.Conditioned as Lang
import qualified Fana.Serial.Bidir.Instances.Maybe as Lang
import qualified Fana.Serial.Bidir.Instances.Multiple as Lang
import qualified Fana.Serial.Bidir.Instances.ProductSum as Lang
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as Data
import qualified Fana.Serial.Bidir.Serializer as Lang
import qualified Fana.Serial.Bidir.Test as SerialTest
import qualified Fana.Serial.Print.Show as Fana


type Text = String
type Serializer p v = Lang.Serializer Char p v v


serializer_inactive :: Fana.ExistsConversion p Char => Serializer p ()
serializer_inactive = Lang.concrete_string "\\ "

serializer_activity :: Fana.ExistsConversion p Char => Serializer p Bool
serializer_activity = 
	let
		from_Maybe :: Maybe () -> Bool
		from_Maybe = Base.isNothing
		to_Maybe :: Bool -> Maybe ()
		to_Maybe True = Nothing
		to_Maybe False = Just ()
	in 
		Profunctor.dimap to_Maybe from_Maybe (Lang.trier serializer_inactive)

serializer_name_char :: Fana.ExistsConversion p Char => Serializer p Char
serializer_name_char = 
	let is_name_char = liftA2 (||) Char.isAlphaNum (== '-')
	in Lang.conditioned is_name_char Lang.atom

serializer_name :: Fana.ExistsConversion p Char => Serializer p Text
serializer_name = Lang.trier_multiple serializer_name_char
		
serializer_value :: Fana.ExistsConversion p Char => Serializer p Text
serializer_value = Lang.trier_multiple Lang.atom

serializer_assignment :: forall p . Fana.ExistsConversion p Char => Serializer p Text
serializer_assignment = 
	let 
		raw_rule :: Serializer p ((), Text)
		raw_rule = Lang.product (Lang.concrete_string " = ", serializer_value)
		iso = Optic.reverse Optic.iso_with_nothing_before
		in Lang.extend_with_iso iso raw_rule

serializer_meaningful_line :: 
	forall p . Fana.ExistsConversion p Char => Serializer p Data.Semantic
serializer_meaningful_line = 
	let
		raw_rule :: Serializer p ((Bool, Text), Maybe Text)
		raw_rule = Lang.product (Lang.product (serializer_activity, serializer_name), Lang.trier serializer_assignment)
		isomorphism :: Optic.Iso' ((Bool, Text), Maybe Text) Data.Semantic
		isomorphism = 
			let
				from_raw :: ((Bool, Text), Maybe Text) -> Data.Semantic
				from_raw ((is_active, name), mb_propert_value) =
					Base.maybe 
						(Data.Semantic is_active name Nothing)
						(\ v -> (Data.Semantic is_active name (Just v)))
						mb_propert_value
				to_raw :: Data.Semantic -> ((Bool, Text), Maybe Text)
				to_raw (Data.Semantic is_active name value) = ((is_active, name), value)
				in Optic.Iso to_raw from_raw
		in Lang.extend_with_iso isomorphism raw_rule

serializer_comment :: forall p . Fana.ExistsConversion p Char => Serializer p Text
serializer_comment = 
	let 
		raw_rule :: Serializer p ((), Text)
		raw_rule = Lang.product (Lang.concrete_string "// ", Lang.trier_multiple Lang.atom)
		iso = Optic.reverse Optic.iso_with_nothing_before
		in Lang.extend_with_iso iso raw_rule

serializer_line :: 
	forall p . (Fana.Showable Text p, Fana.ExistsConversion p Char) => Serializer p Data.Node
serializer_line =
	let
		raw_rule :: Serializer p (Either Text Data.Semantic)
		raw_rule = Lang.sum (serializer_comment, serializer_meaningful_line)
		isomorphism :: Optic.Iso' (Either Text Data.Semantic) Data.Node
		isomorphism = 
			let
				to_raw = Data.process_Node Right Left
				from_raw = Base.either Data.MakeComment Data.MakeSemantic
				in Optic.Iso to_raw from_raw
			in Lang.whole (Lang.extend_with_iso isomorphism raw_rule)


-------------------------- TESTS ----------------------------------

serializer_inactive_test :: Test
serializer_inactive_test = 
	Test.single "serializer_inactive"  (SerialTest.test_serializer serializer_inactive [()] ["\\"])

serializer_activity_test :: Test
serializer_activity_test = 
	Test.single "serializer_active_raw"  (SerialTest.test_serializer serializer_activity [True, False] [])

serializer_name_test :: Test
serializer_name_test = 
	Test.single "serializer_name" (SerialTest.test_serializer serializer_name ["barbara", "jane-doe"] [])

serializer_value_test :: Test
serializer_value_test = 
	Test.single "serializer_value" (SerialTest.test_serializer serializer_value ["", "ah", "\" \""] [])

serializer_assignment_test :: Test
serializer_assignment_test = 
	Test.single "serializer_assignment" 
		(SerialTest.test_serializer serializer_assignment ["4", "ah"] ["=fld", " =kdjh", "= jfh"])

serializer_meaningful_line_test :: Test
serializer_meaningful_line_test = 
	Test.single "serializer_meaningful_line" 
		(
			SerialTest.test_serializer serializer_meaningful_line
				[ Data.Semantic True "food" Nothing
				, Data.Semantic False "food" (Just "apple")
				]
				[]
		)

serializer_line_test :: Test
serializer_line_test = 
	Test.single "serializer_line"
		(
			SerialTest.test_serializer serializer_line
				[ Data.MakeComment "comm"
				, Data.MakeSemantic (Data.Semantic True "apple" Nothing)
				]
				["/ "]
		)


test :: Test.Test
test = 
	let 
		simple_tests :: [Test]
		simple_tests = 
			[ serializer_inactive_test, serializer_activity_test
			, serializer_name_test, serializer_value_test, serializer_assignment_test
			, serializer_meaningful_line_test
			, serializer_line_test
			]
		in Test.bunch "lines" simple_tests
