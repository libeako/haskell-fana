-- | Serialization of the Simco language (to | from) indented text.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.IndentedTextSerial
(
	ParseError, serializer,
)
where

import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Int, (+), Char, String)

import qualified Data.Foldable as Base
import qualified Data.Functor.Compose as Base
import qualified Data.List as List
import qualified Data.Traversable as Traversable
import qualified Data.Tree as Base
import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Serial
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as Data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.NodeSerial as NodeSerial
import qualified Fana.Serial.Bidir.Parse as Serial
import qualified Fana.Serial.Bidir.Serializer as Serial
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show


type Text = String

data LineIdentifier = LineIdentifier { line_ordinal :: Int, line_content :: Text }

instance Fana.Showable Text LineIdentifier where
	show l =
		mempty
		<> "line " <> Show.from_Show (line_ordinal l)
		<> " [\"" <> Accu.single (line_content l) <> "\"]"

data ParseError
	= ErrorInHightListParsing (TreeSerial.HightListParseError Text)
	| ErrorInLineParsing LineIdentifier [Serial.Error Char]

instance Fana.Showable Text ParseError where
	show = \case
		ErrorInHightListParsing details -> "error in hight list parsing : " <> Fana.show details
		ErrorInLineParsing line errors -> 
			mempty
			<> "error in line parsing in " <> Fana.show line 
			<> " : ["
			<> Base.fold (List.intersperse ", " (map Fana.show errors))
			<> "]"

type Serializer l1 l2 h1 h2 = Optic.PartialIso ParseError l1 l2 h1 h2
type Serializer' l h = Serializer l l h h

serializer_text_forest :: Serializer' Text [Base.Tree Text]
serializer_text_forest = Optic.piso_convert_error ErrorInHightListParsing Serial.text_tree

serializer_node :: Optic.PartialIso' [Serial.Error Char] Text Data.NodeWithActivity
serializer_node = Serial.to_partial_iso (NodeSerial.serializer)

serializer_identified_line :: Serializer Text (LineIdentifier, Text) Data.NodeWithActivity Data.NodeWithActivity
serializer_identified_line = Optic.piso_convert_error (uncurry ErrorInLineParsing) (Optic.add_for_failure serializer_node)

serializer_lines_general :: Traversable t => Serializer (t Text) (t (LineIdentifier, Text)) (t Data.NodeWithActivity) (t Data.NodeWithActivity)
serializer_lines_general = Optic.lift_piso serializer_identified_line

identify_lines :: Traversable t => t Text -> t (LineIdentifier, Text)
identify_lines = 
	let
		accumulator :: Int -> Text -> (Int, (LineIdentifier, Text))
		accumulator count text = (count +1, ((LineIdentifier count text), text))
		in Traversable.mapAccumL accumulator 1 >>> snd

serializer_lines :: 
	Serializer 
		Text Text
		(Base.Compose [] Base.Tree Text) (Base.Compose [] Base.Tree (LineIdentifier, Text))
serializer_lines = 
	Optic.piso_convert_all id id (Base.Compose >>> identify_lines) Base.getCompose id
	serializer_text_forest

serializer :: Serializer' Text (Base.Forest Data.NodeWithActivity)
serializer = 
	Optic.piso_convert_all id id Base.getCompose Base.Compose id
	(serializer_lines >**> serializer_lines_general)
