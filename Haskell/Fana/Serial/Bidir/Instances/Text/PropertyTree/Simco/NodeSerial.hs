module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.NodeSerial
(
	serializer,
)
where

import Fana.Prelude
import Prelude (Char, String)

import qualified Data.Char as Char
import qualified Data.Either as Base
import qualified Data.Maybe as Base
import qualified Fana.Convert as Fana
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Instances.Basic as Serial
import qualified Fana.Serial.Bidir.Instances.Concrete as Serial
import qualified Fana.Serial.Bidir.Instances.Conditioned as Serial
import qualified Fana.Serial.Bidir.Instances.Maybe as Serial
import qualified Fana.Serial.Bidir.Instances.Multiple as Serial
import qualified Fana.Serial.Bidir.Instances.ProductSum as Serial
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as Data
import qualified Fana.Serial.Bidir.Serializer as Serial
import qualified Fana.Serial.Print.Show as Fana


type Text = String
type Serializer p v = Serial.Serializer Char p v v


-- | Serializer of a character in a name.
serializer_name_char :: Fana.ExistsConversion p Char => Serializer p Char
serializer_name_char = 
	let is_name_char = liftA2 (||) Char.isAlphaNum (== '-')
		in Serial.conditioned is_name_char Serial.atom

serializer_name :: Fana.ExistsConversion p Char => Serializer p Text
serializer_name = Serial.trier_multiple serializer_name_char

-- | Serializer of an atomic property value.
serializer_value :: Fana.ExistsConversion p Char => Serializer p Text
serializer_value = Serial.trier_multiple Serial.atom

-- | Serializer of the assignment symbol.
serializer_assignment :: Fana.ExistsConversion p Char => Serializer p Text
serializer_assignment =
	let
		raw :: Fana.ExistsConversion p Char => Serializer p ((), Text)
		raw = Serial.product (Serial.concrete_string " = ", serializer_value)
		in Serial.extend_with_iso (Optic.reverse Optic.iso_with_nothing_before) raw

-- | Serialzier of an important node.
serializer_important :: forall p . Fana.ExistsConversion p Char => Serializer p Data.ImportantNode
serializer_important =
	let
		raw :: Serializer p (Text, Maybe Text)
		raw = Serial.product (serializer_name, Serial.trier serializer_assignment)
		from_raw :: (Text, Maybe Text) -> Data.ImportantNode
		from_raw (name, mb_propert_value) =
			Base.maybe 
				(Data.ImportantNode name Nothing)
				(Just >>> Data.ImportantNode name)
				mb_propert_value
		to_raw :: Data.ImportantNode -> (Text, Maybe Text)
		to_raw (Data.ImportantNode name value) = (name, value)
		in Serial.extend_with_iso (Optic.Iso to_raw from_raw) raw

-- | Serializer of a comment node.
serializer_comment :: forall p . Fana.ExistsConversion p Char => Serializer p Text
serializer_comment =
	let
		raw :: Serializer p ((), Text)
		raw = Serial.product (Serial.concrete_string "// ", Serial.trier_multiple Serial.atom)
		iso = Optic.reverse Optic.iso_with_nothing_before
		in Serial.extend_with_iso iso raw


-- * About activity

-- | Serializer of an active node.
serializer_active :: forall p . (Fana.Showable Text p, Fana.ExistsConversion p Char) => Serializer p Data.ActiveNode
serializer_active =
	let
		raw :: Serializer p (Either Text Data.ImportantNode)
		raw = Serial.sum (serializer_comment, serializer_important)
		iso :: Optic.Iso' (Either Text Data.ImportantNode) Data.ActiveNode
		iso =
			let
				to_raw = Data.process_ActiveNode Right Left
				from_raw = Base.either Data.MakeComment Data.MakeImportant
				in Optic.Iso to_raw from_raw
		in Serial.whole (Serial.extend_with_iso iso raw)

-- | Serializer of the inactivity symbol.
serializer_inactivity_symbol :: Fana.ExistsConversion p Char => Serializer p ()
serializer_inactivity_symbol = Serial.concrete_string "\\ "

activity_maybe_iso :: Optic.Iso' (Maybe ()) Data.Activity
activity_maybe_iso =
	let
		to_Maybe Data.Active = Nothing
		to_Maybe Data.InActive = Just ()
		from_Maybe Nothing = Data.Active
		from_Maybe (Just _) = Data.InActive
		in Optic.Iso to_Maybe from_Maybe

serializer_activity :: Fana.ExistsConversion p Char => Serializer p Data.Activity
serializer_activity = Serial.extend_with_iso activity_maybe_iso (Serial.trier serializer_inactivity_symbol)


-- | Serializer of a whole node.
serializer :: forall p . (Fana.Showable Text p, Fana.ExistsConversion p Char) => Serializer p Data.NodeWithActivity
serializer =
	let
		raw :: Serializer p (Data.Activity,  Data.ActiveNode)
		raw = Serial.product (serializer_activity, serializer_active)
		in Serial.whole raw
