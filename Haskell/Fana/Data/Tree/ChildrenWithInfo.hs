{-|
	Tree data structure where the children-list [the "forest"] has some additional info.
-}

module Fana.Data.Tree.ChildrenWithInfo
(
	List (..), Forest, Tree (..),
	trunk_in_Tree, subforest_in_Tree, 
	additionals_in_Tree, additionals_in_Forest,
)
where

import Fana.Prelude

import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified Fana.Optic.Concrete.Categories.Traversal as Optic

type List a e = (a, [e])

type Forest a e = List a (Tree a e)

data Tree a e =
	Node
	{
		trunk :: e,
		forest :: Forest a e
	}
	deriving (Eq, Functor, Foldable, Traversable)



{- --------------------- optics ------------------------- -}


trunk_in_Tree :: Optic.Lens' e (Tree a e)
trunk_in_Tree = Optic.lens_from_get_set trunk (\ u e -> e { trunk = u })

subforest_in_Tree :: Optic.Lens' (Forest a e) (Tree a e)
subforest_in_Tree = Optic.lens_from_get_set forest (\ f t -> t { forest = f })


traverse_additionals_in_Forest :: Applicative f => (a1 -> f a2) -> Forest a1 e -> f (Forest a2 e)
traverse_additionals_in_Forest fa (a, c) = liftA2 (,) (fa a) (traverse (traverse_additionals_in_Tree fa) c)

traverse_additionals_in_Tree :: Applicative f => (a1 -> f a2) -> Tree a1 e -> f (Tree a2 e)
traverse_additionals_in_Tree fa (Node t sf) = map (Node t) (traverse_additionals_in_Forest fa sf)

additionals_in_Forest :: forall a1 a2 e . Optic.Traversal a1 a2 (Forest a1 e) (Forest a2 e)
additionals_in_Forest = Optic.Traversal traverse_additionals_in_Forest

additionals_in_Tree :: forall a1 a2 e . Optic.Traversal a1 a2 (Tree a1 e) (Tree a2 e)
additionals_in_Tree = Optic.Traversal traverse_additionals_in_Tree
