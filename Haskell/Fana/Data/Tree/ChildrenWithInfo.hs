{-|
	Tree data structure where the children-list [the "forest"] has some additional info.
-}

module Fana.Data.Tree.ChildrenWithInfo
(
	List (..), Forest, Tree (..),
	trunk_in_Tree, subforest_in_Tree,
)
where

import Fana.Prelude

import qualified Fana.Optic.Concrete.Categories.Lens as Optic


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
