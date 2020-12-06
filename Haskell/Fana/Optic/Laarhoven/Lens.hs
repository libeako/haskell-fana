module Fana.Optic.Laarhoven.Lens
(
	LensNaked, LensNaked',
	purify_lens,
	get, set,
	-- * Conversions to|from concrete representation
	to_concrete, from_concrete,
)
where

import Control.Applicative (Const (..))
import Data.Functor.Identity(Identity (..))

import Fana.Prelude hiding (flip)

import qualified Prelude as Base
import qualified Fana.Optic.Concrete.Categories.Lens as Concrete


type LensNaked p1 p2 w1 w2 = forall f . Functor f => (p1 -> f p2) -> (w1 -> f w2)
type LensNaked' p w = LensNaked p p w w

newtype LensOutput p1 p2 w2 =
	LensOutput { unwrapLensOutput :: forall f . Functor f => (p1 -> f p2) -> f w2 }


-- | Moves w1 out from the universal functor quantification.
-- It is the same as the regular function flip,
-- but on the Laarhoven representation of a Lens, which is also a function.
flip :: LensNaked p1 p2 w1 w2 -> w1 -> LensOutput p1 p2 w2
flip l w1 = LensOutput (Base.flip l w1)

-- | Simplifies the given lens, taking aways from it its ability to perform effects.
purify_lens :: LensNaked p1 p2 w1 w2 -> (p1 -> p2) -> (w1 -> w2)
purify_lens lens = (>>> Identity) >>> lens >>> (>>> runIdentity)

get_from_output :: LensOutput p1 p2 w2 -> p1
get_from_output l = getConst (unwrapLensOutput l Const)

-- | Extracts a getter function from the given lens.
get :: LensNaked p1 p2 w1 w2 -> w1 -> p1
get lens w = get_from_output (flip lens w)

-- | Extracts a setter function from the given lens.
set :: LensNaked p1 p2 w1 w2 -> p2 -> (w1 -> w2)
set lens p = purify_lens lens (const p)


-- Conversions to|from concrete representation :

from_concrete :: Concrete.Lens p1 p2 w1 w2 -> LensNaked p1 p2 w1 w2
from_concrete c pfp w = map (snd r) p2
	where
		r = Concrete.lens_to_sety_function c w
		p2 = pfp (fst r)

to_concrete :: forall p1 p2 w1 w2 . LensNaked p1 p2 w1 w2 -> Concrete.Lens p1 p2 w1 w2
to_concrete lens = get_Lens (flip lens)
	where
		get_mappy :: LensOutput p1 p2 w2 -> (p1 -> p2) -> w2
		get_mappy (LensOutput l) = (map Identity >>>) (map runIdentity l)
		get_both :: LensOutput p1 p2 w2 -> (p1, (p1 -> p2) -> w2)
		get_both l = (get_from_output l, get_mappy l)
		get_Lens :: (w1 -> LensOutput p1 p2 w2) -> Concrete.Lens p1 p2 w1 w2
		get_Lens =
			map (get_both >>> Concrete.lens_output_from_lift_to_set)
			>>> Concrete.lens_from_sety_function
