module Fana.Serial.Bidir.Instances.Enum
(
	set, enum,
)
where

import Prelude (Ord)
import Fana.Prelude.FromBase

import qualified Fana.Data.Enum as Enum
import qualified Fana.Data.Key.LensToMaybeElement as LensAt
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as MapS
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


{-|
	Creates a serializer for the given set of values,
	specified by the given renderer.
-}
set ::
	forall c v .
	Ord c =>
	(v -> [c]) -> [v] -> Optic.PartialIso' () [c] v
set render values =
	let
		pair_list = map (liftA2 (,) render id) values
		dict :: MapS.Map c v
		dict = MapI.from_list pair_list
		interpret :: [c] -> Either () v
		interpret string =
			maybe (Left ()) Right (LensAt.get_at string dict)
		in Optic.PartialIso render interpret

{-|
	Creates a serializer for a type of enumerable values,
	specified by the given renderer.
-}
enum ::
	forall c v .
	(Ord c, Base.Enum v, Base.Bounded v) =>
	(v -> [c]) -> Optic.PartialIso' () [c] v
enum render = set render Enum.all_values
