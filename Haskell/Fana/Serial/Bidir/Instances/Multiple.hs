module Fana.Serial.Bidir.Instances.Multiple
(
	trier_multiple,
)
where

import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Parse (Parser)

import qualified Data.Foldable as Fold
import qualified Fana.Serial.Bidir.Instances.Maybe as Rules


{-|
	Renders the given value, independently from each other.
	Does not write the number of elements to be written.
	So this is not a real renderer of a list of values.
-}
render_multiple :: Renderer c v -> Renderer c [v]
render_multiple = Fold.foldMap

{-|
	Parses values till it fails.
	It does not really know how many elements to read,
	So this is not a real parser of a list of values.
-}
parse_till_can :: Parser p v -> Parser p [v]
parse_till_can p =
	do
		mbv <- Rules.parse_if_can p
		case mbv of
			Nothing -> return []
			Just v -> map (v :) (parse_till_can p)

{-|
	Possibly empty sequence of instances of the same serialization, lasting till can.

	It does not store the number of elements of the list.
	It just keeps writing and keeps reading till it can.
	Thus it is not a real serializer of the list type.
-}
trier_multiple :: forall c c' x x' . Serializer c c' x x' -> Serializer c c' [x] [x']
trier_multiple = transform render_multiple parse_till_can
