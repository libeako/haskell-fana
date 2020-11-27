{-# LANGUAGE TypeOperators #-}

module Fana.Optic.Profunctor.Equality
(
	Equality,
	Equality',

	-- $convert-base
	to_base_eq, from_base_eq,
)
where

import           Control.Category
import qualified Data.Type.Equality as Base
import           Data.Type.Equality ((:~:))

import           Fana.Optic.Profunctor.Optic

type Equality p1 p2 w1 w2 = forall tf . OpticBone tf p1 p2 w1 w2
type Equality' p w = TypeKeeper Equality p w

-- | Same as Data.Type.Equality.(:~:),
-- but for 2 pairs of type variables simultaniously.
data ConcreteEqual x y x' y' where
	Refl :: ConcreteEqual x y x y

-- $convert-base
--
-- conversion to,from the type level equality in library 'base'

to_base_eq :: Equality p1 p2 w1 w2 -> (p1 :~: w1, p2 :~: w2)
to_base_eq o = case o Refl of Refl -> (Base.Refl, Base.Refl)

from_base_eq :: (p1 :~: w1, p2 :~: w2) -> Equality p1 p2 w1 w2
from_base_eq (Base.Refl, Base.Refl) = id
