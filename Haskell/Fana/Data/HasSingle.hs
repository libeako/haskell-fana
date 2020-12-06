-- | interface for containers with exactly one element.
module Fana.Data.HasSingle
(
	HasSingle (..),
	lens,
	move_in, move_out, iso_separate,
)
where

import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Fana.Prelude

import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Prelude as Base


-- | Monomorphic version of 'HasSingle'.
class HasSingleMono (c :: Type) where
	type SingleMono c :: Type
	mono_elem :: c -> SingleMono c

-- | Interface for containers with exactly one element.
class Functor c => HasSingle (c :: Type -> Type) where elem :: c e -> e

instance HasSingle Identity where elem = runIdentity
instance HasSingle ((,) a) where elem = Base.snd

lens :: HasSingle c => Optic.Lens e1 e2 (c e1) (c e2)
lens = Optic.lens_from_get_lift elem map


move_in :: Functor a => (a (), e) -> a e
move_in (a, e) = a $> e

move_out :: HasSingle a => a e -> (a (), e)
move_out a = (a $> (), elem a)

iso_separate :: (Functor a1, HasSingle a2) => Optic.Iso (a1 e1) (a2 e2) (a1 (), e1) (a2 (), e2)
iso_separate = Optic.Iso move_in move_out
