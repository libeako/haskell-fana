module Fana.Optic.Profunctor.AdaptIso
(
	iso_adapt,
)
where

import Fana.Optic.Profunctor.Interface (Adapted(..))
import Fana.Optic.Profunctor.Optic (OpticWrapped(..))
import qualified Fana.Optic.Concrete.Categories.Iso as Iso
import qualified Fana.Optic.Profunctor.Interface as If


iso_adapt ::
	Adapted o =>
	Iso.Iso
		(o i11 i12 o11 o12)
		(o i21 i22 o21 o22)
		(OpticWrapped (ProfunctorConstraint o) i11 i12 o11 o12)
		(OpticWrapped (ProfunctorConstraint o) i21 i22 o21 o22)
iso_adapt = Iso.Iso (If.wrapped_to_adapted) (If.wrapped_from_adapted)

