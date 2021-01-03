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
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Instances.Basic as Serial
import qualified Fana.Serial.Bidir.Instances.Concrete as Serial
import qualified Fana.Serial.Bidir.Instances.Conditioned as Serial
import qualified Fana.Serial.Bidir.Instances.Maybe as Serial
import qualified Fana.Serial.Bidir.Instances.Multiple as Serial
import qualified Fana.Serial.Bidir.Instances.ProductSum as Serial
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as Data
import qualified Fana.Serial.Bidir.Serializer as Serial
import qualified Fana.Serial.Bidir.Test as SerialTest
import qualified Fana.Serial.Print.Show as Fana


type Text = String
type Serializer p v = Serial.Serializer Char p v v


serializer_inactive :: Fana.ExistsConversion p Char => Serializer p ()
serializer_inactive = Serial.concrete_string "\\ "

bool_maybe_iso :: Optic.Iso' (Maybe ()) Bool
bool_maybe_iso = 
	let
		to_Maybe True = Nothing
		to_Maybe False = Just ()
		in Optic.Iso to_Maybe Base.isNothing

serializer_activity :: Fana.ExistsConversion p Char => Serializer p Bool
serializer_activity = Serial.extend_with_iso bool_maybe_iso (Serial.trier serializer_inactive)

serializer_name_char :: Fana.ExistsConversion p Char => Serializer p Char
serializer_name_char = 
	let is_name_char = liftA2 (||) Char.isAlphaNum (== '-')
		in Serial.conditioned is_name_char Serial.atom

serializer_name :: Fana.ExistsConversion p Char => Serializer p Text
serializer_name = Serial.trier_multiple serializer_name_char
		
serializer_value :: Fana.ExistsConversion p Char => Serializer p Text
serializer_value = Serial.trier_multiple Serial.atom

serializer_assignment :: Fana.ExistsConversion p Char => Serializer p Text
serializer_assignment = 
	let
		raw :: Fana.ExistsConversion p Char => Serializer p ((), Text)
		raw = Serial.product (Serial.concrete_string " = ", serializer_value)
		in Serial.extend_with_iso (Optic.reverse Optic.iso_with_nothing_before) raw

serializer_semantic_line :: forall p . Fana.ExistsConversion p Char => Serializer p Data.Semantic
serializer_semantic_line = 
	let
		raw :: Serializer p ((Bool, Text), Maybe Text)
		raw = Serial.product (Serial.product (serializer_activity, serializer_name), Serial.trier serializer_assignment)
		from_raw :: ((Bool, Text), Maybe Text) -> Data.Semantic
		from_raw ((is_active, name), mb_propert_value) =
			Base.maybe 
				(Data.Semantic is_active name Nothing)
				(Just >>> Data.Semantic is_active name)
				mb_propert_value
		to_raw :: Data.Semantic -> ((Bool, Text), Maybe Text)
		to_raw (Data.Semantic is_active name value) = ((is_active, name), value)
		in Serial.extend_with_iso (Optic.Iso to_raw from_raw) raw

serializer_comment :: forall p . Fana.ExistsConversion p Char => Serializer p Text
serializer_comment = 
	let 
		raw :: Serializer p ((), Text)
		raw = Serial.product (Serial.concrete_string "// ", Serial.trier_multiple Serial.atom)
		iso = Optic.reverse Optic.iso_with_nothing_before
		in Serial.extend_with_iso iso raw

serializer_line :: forall p . (Fana.Showable Text p, Fana.ExistsConversion p Char) => Serializer p Data.Node
serializer_line =
	let
		raw :: Serializer p (Either Text Data.Semantic)
		raw = Serial.sum (serializer_comment, serializer_semantic_line)
		iso :: Optic.Iso' (Either Text Data.Semantic) Data.Node
		iso = 
			let
				to_raw = Data.process_Node Right Left
				from_raw = Base.either Data.MakeComment Data.MakeSemantic
				in Optic.Iso to_raw from_raw
		in Serial.whole (Serial.extend_with_iso iso raw)


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
	Test.single "serializer_semantic_line" 
		(
			SerialTest.test_serializer serializer_semantic_line
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
