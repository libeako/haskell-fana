module Fana.Optic.Laarhoven.Traverse
(
	Traversal (..),
	Wandering (..),
)
where

import Data.Functor.Identity (Identity (..))
import Fana.Math.Algebra.Category.Functor.Pro (Profunctor)
import Fana.Prelude

import qualified Data.Constraint as Constraint
import qualified Data.Traversable as Trav
import qualified Fana.Data.FunctionUpIo as FunUpIo
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Categories.Interfaces as Concrete
import qualified Fana.Optic.Concrete.Categories.Iso as Concrete
import qualified Fana.Optic.Profunctor.Optic as OpticP
import qualified Fana.Optic.Profunctor.Interface as OpticP


type TraversalNude p1 p2 w1 w2 = forall f . Applicative f => (p1 -> f p2) -> (w1 -> f w2)
data Traversal p1 p2 w1 w2 = Traversal { unwrapTraversal :: TraversalNude p1 p2 w1 w2 }

{-|
	Simplifies the given traversal,
	taking away from it its ability to perform effects.
-}
to_map :: TraversalNude p1 p2 w1 w2 -> ((p1 -> p2) -> (w1 -> w2))
to_map = Profunctor.dimap (>>> Identity) (>>> runIdentity)

instance Cat2.Category Traversal where
	empty = Traversal id
	compose ::
		Traversal p1 p2 q1 q2 ->
		Traversal q1 q2 r1 r2 ->
		Traversal p1 p2 r1 r2
	compose pq qr = Traversal (unwrapTraversal pq >>> unwrapTraversal qr)

class Profunctor.LoadablePS p => Wandering p where
	wander_lift :: Traversable t => p x y -> p (t x) (t y)
	wander_lift = wander (Traversal Trav.traverse)
	wander :: Traversal p1 p2 w1 w2 -> OpticP.OpticBone p p1 p2 w1 w2

instance Wandering (->) where wander t = map Identity >>> unwrapTraversal t >>> map runIdentity
instance Applicative f => Wandering (FunUpIo.UpO f) where
	wander = unwrapTraversal >>> Concrete.fn_up FunUpIo.iso_UpO

map_w_in_Traversal ::
	(forall f . Applicative f => (w1i -> f w2i) -> (w1o -> f w2o)) ->
	Traversal p1 p2 w1i w2i ->
	Traversal p1 p2 w1o w2o
map_w_in_Traversal f trav = Traversal (map f (unwrapTraversal trav))

map_w_TravFunL ::
	(forall f . Applicative f => FunUpIo.UpO f w1i w2i -> FunUpIo.UpO f w1o w2o) ->
	Traversal p1 p2 w1i w2i ->
	Traversal p1 p2 w1o w2o
map_w_TravFunL f = map_w_in_Traversal (Concrete.fn_down FunUpIo.iso_UpO f)

instance Profunctor (Traversal p1 p2) where dimap f g = map_w_TravFunL (Profunctor.dimap f g)
instance Profunctor.LoadableP (Traversal p1 p2) where load_pe = map_w_TravFunL Profunctor.load_pe
instance Profunctor.LoadableS (Traversal p1 p2) where load_se = map_w_TravFunL Profunctor.load_se
instance Profunctor.LoadablePS (Traversal p1 p2) where
instance Wandering (Traversal p1 p2) where
	wander pw pp = Traversal (unwrapTraversal pp >>> unwrapTraversal pw)
	wander_lift (Traversal trav) = Traversal (trav >>> (map >>> (>>> Trav.sequenceA)))

instance OpticP.Adapted Traversal where
	type ProfunctorConstraint Traversal = Wandering
	proof_of_constraint_implementation = Constraint.Dict
	from_adapted = wander
