module Fana.Optic.Concrete.Combine
(
	Productable (..), Sumable (..),
)
where


import Prelude (Either)

import Fana.Optic.Concrete.Common


class Productable (o :: Optic) where
	product ::
		forall l1 l2 h11 h12 h21 h22 .
		(o l1 l2 h11 h12, o l1 l2 h21 h22) -> o l1 l2 (h11, h21) (h12, h22)
class Sumable (o :: Optic) where
	sum ::
		forall l1 l2 h11 h12 h21 h22 .
		(o l1 l2 h11 h12, o l1 l2 h21 h22) -> o l1 l2 (Either h11 h21) (Either h12 h22)
