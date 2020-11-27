module Fana.Serial.Bidir.Instances.Conditioned
(
	conditioned,
)
where

import Fana.Convert (ExistsConversion (..))
import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Serializer
import Prelude (String)

import qualified Fana.Data.Function as Fn
import qualified Fana.Optic.Concrete.Prelude as Optic


{-|
	Restricts the parser with the given condition.
	Parsed values that do not met the condition are rejected and result in error.
	The renderer does not check the condition,
	that is left for the client coder.
-}
conditioned :: forall c c' v v' . ExistsConversion v' v => (v -> Bool) -> Fn.Endo (Serializer c c' v v')
conditioned condition =
	let
		extension :: Optic.PartialIso String v v' v v'
		extension =
			let
				interpret e =
					if condition (convert e) then Right e else Left "condition on parsed value not met"
				in Optic.PartialIso id interpret
		in extend_with_partial_iso extension
