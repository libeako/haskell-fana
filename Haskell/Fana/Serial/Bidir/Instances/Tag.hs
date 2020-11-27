module Fana.Serial.Bidir.Instances.Tag
(
	tagged,
)
where

import Fana.Convert (ExistsConversion (..))
import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Instances.Concrete
import Fana.Serial.Bidir.Instances.Decorate (decorate)
import Fana.Serial.Bidir.Serializer

import qualified Fana.Data.Function as Fn
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Optic.Concrete.Categories.Iso as Iso
import qualified Fana.Serial.Bidir.Instances.ProductSum as PS


{- |
	Creates a tag for the given serializer in the stream.
	The given string is the tag.
	The tag is a herald of the given serialization,
	its purpose is to tell what is after it in the stream.
-}
tagged :: forall c c' x x' . (Eq c, ExistsConversion c' c) => [c] -> Fn.Endo (Serializer c c' x x')
tagged tag rule_value =
	let
		prod :: Serializer c c' ((), x) ((), x')
		prod =
			PS.product ((decorate "tag" (concrete_string tag)), (decorate "tag-constrained" rule_value))
		tagging :: Iso.Iso ((), x) ((), x') x x'
		tagging = Iso.Iso (Pair.after ()) snd
		in extend_with_iso tagging prod
