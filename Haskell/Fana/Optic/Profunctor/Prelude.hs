module Fana.Optic.Profunctor.Prelude
(
	OpticBone, OpticNude, TypeKeeper,
	from_adapted, to_adapted,
	get, match,
	set, build,
	over, traverse,
)
where

import Fana.Optic.Profunctor.Optic
import Fana.Optic.Profunctor.Interface
import Fana.Optic.Profunctor.Access.Get
import Fana.Optic.Profunctor.Access.Set
import Fana.Optic.Profunctor.Access.Over
