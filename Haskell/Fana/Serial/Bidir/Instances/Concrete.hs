module Fana.Serial.Bidir.Instances.Concrete
(
	concrete_atom,
	concrete_string,
)
where

import Fana.Convert (ExistsConversion (..))
import Fana.Prelude
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Parse (Parser, ErrorText)

import qualified Fana.Data.Tree.Leaf as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Basic as Basic


concrete_atom :: forall c c' . (Eq c, ExistsConversion c' c) => c -> Serializer c c' () ()
concrete_atom c =
	let
		validate :: c -> Either ErrorText ()
		validate =
			convert >>> (== c) >>>
			\ case { True -> Right (); False -> Left "unexpected atom" }
		in extend_with_partial_iso (Optic.PartialIso (const c) validate) Basic.atom

parse_concrete_string :: forall p c . (Eq c, ExistsConversion p c) => [c] -> Parser p ()
parse_concrete_string = map (concrete_atom >>> parser) >>> sequenceA >>> map (const ())

render_concrete_string :: [c] -> Renderer c ()
render_concrete_string = map Tree.leaf >>> Tree.joint >>> (: []) >>> const

concrete_string :: (Eq c, ExistsConversion c' c) => [c] -> Serializer c c' () ()
concrete_string = liftA2 Serializer render_concrete_string parse_concrete_string
