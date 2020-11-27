module Fana.Optic.Profunctor.Interface
(
	Adapted (..),
	wrapped_from_adapted,
	wrapped_to_adapted,
)
where

import Data.Kind (Type, Constraint)
import Fana.Optic.Profunctor.Optic (OpticNude, OpticWrapped (..))

import qualified Data.Constraint as Constraint
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2


class Cat2.Category o => Adapted (o :: Type -> Type -> Type -> Type -> Type) where
	type ProfunctorConstraint o :: (Type -> Type -> Type) -> Constraint
	proof_of_constraint_implementation :: Constraint.Dict (ProfunctorConstraint o (o p1 p2))
	from_adapted :: o i1 i2 o1 o2 -> (OpticNude (ProfunctorConstraint o) i1 i2 o1 o2)
	to_adapted :: forall p1 p2 w1 w2 .
		(OpticNude (ProfunctorConstraint o) p1 p2 w1 w2 -> o p1 p2 w1 w2)
	to_adapted a =
		let
			dict :: Constraint.Dict (ProfunctorConstraint o (o p1 p2))
			dict = proof_of_constraint_implementation
			-- the concrete optic transformer :
			c :: o p1 p2 p1 p2 -> o p1 p2 w1 w2
			c = Constraint.withDict dict a
		in c Cat2.empty


-- | the following converter functions wrap type inference problem

wrapped_from_adapted :: Adapted o => o i1 i2 o1 o2 -> (OpticWrapped (ProfunctorConstraint o) i1 i2 o1 o2)
wrapped_from_adapted o = Wrap (from_adapted o)

wrapped_to_adapted :: Adapted o => (OpticWrapped (ProfunctorConstraint o) i1 i2 o1 o2 -> o i1 i2 o1 o2)
wrapped_to_adapted o = to_adapted (unwrapOptic o)


