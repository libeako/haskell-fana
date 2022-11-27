-- | Serialization of the Simco language (to | from) indented text.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.AsText
(
	serializer_with_line_ordinal, serializer,
)
where

import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Fana.Serial.Bidir.Instances.Text.Indent (Count)
import Prelude (Char, String)

import qualified Data.Tree as Base
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Indent
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as Data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.NodeSerial as NodeSerial
import qualified Fana.Serial.Bidir.Parse as Serial
import qualified Fana.Serial.Bidir.Serializer as Serial
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base


serializer_of_node :: Optic.PartialIso' String String Data.NodeWithActivity
serializer_of_node = 
	let
		render_error_message :: [Serial.Error Char] -> String
		render_error_message = List.first >>> maybe "" Fana.show >>> Acc.extract
		in Optic.piso_convert_error render_error_message (Serial.to_partial_iso (NodeSerial.serializer))

serializer_of_counted_node ::
	Optic.PartialIso String 
		String (Count, String) 
		Data.NodeWithActivity (Count, Data.NodeWithActivity)
serializer_of_counted_node = 
	let
		Optic.PartialIso r p = serializer_of_node
		new_p :: (Count, String) -> Either String (Count, Data.NodeWithActivity)
		new_p (c, s) =
			let
				extend_error_message :: String -> String
				extend_error_message em = "SimCo language format error in line " <> Base.show c
				in bimap extend_error_message (Pair.after c) (p s)
		in Optic.PartialIso r new_p

lift_parse :: Traversable tr => (x -> Either e y) -> (tr x -> Either e (tr y))
lift_parse f = map f >>> sequenceA

serializer_lines_tree ::
	forall ti to .
	(Traversable ti, Traversable to) => 
	Optic.PartialIso String
		(to (ti String)) (to (ti (Count, String)))
		(to (ti Data.NodeWithActivity)) (to (ti (Count, Data.NodeWithActivity)))
serializer_lines_tree = 
	let
		p :: (Count, String) -> Either String (Count, Data.NodeWithActivity)
		Optic.PartialIso r p = serializer_of_counted_node
		in Optic.PartialIso ((map >>> map) r) ((lift_parse >>> lift_parse) p)

serializer_with_line_ordinal ::
	Optic.PartialIso String
		String String
		(Base.Forest Data.NodeWithActivity)
		(Base.Forest (Count, Data.NodeWithActivity))
serializer_with_line_ordinal = Indent.text_tree >**> serializer_lines_tree

serializer ::
	Optic.PartialIso String
		String String
		(Base.Forest Data.NodeWithActivity)
		(Base.Forest Data.NodeWithActivity)
serializer =
	let Optic.PartialIso r p = serializer_with_line_ordinal
		in Optic.PartialIso r (p >>> (map >>> map >>> map) snd)  

