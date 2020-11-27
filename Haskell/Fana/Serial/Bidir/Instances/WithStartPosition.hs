module Fana.Serial.Bidir.Instances.WithStartPosition
(
	with_start_position,
)
where

import Fana.Prelude
import Fana.Serial.Bidir.Parse (Parser)

import qualified Control.Monad.State as Mtl
import qualified Data.Maybe as Base


{-|
	Saves the start positition of parsing.

	A parser is given,
	with a dependency to the position of the first element of its input,
	helping in creating an informative error message.
	This modifier saves the start position
	and feeds it into the given parser as its required dependency.
-}
with_start_position :: (Maybe c -> Parser c v) -> Parser c v
with_start_position f = Mtl.get >>= (Base.listToMaybe >>> f)

