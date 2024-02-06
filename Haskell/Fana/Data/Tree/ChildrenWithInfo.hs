{-|
	Tree data structure where the children-list [the "forest"] has some additional info.
-}

module Fana.Data.Tree.ChildrenWithInfo
(
	Forest (..), Tree (..),
	additional_in_Forest, trees_in_Forest, trunk_in_Tree, forest_in_Tree,
)
where


import Fana.Prelude

import qualified Fana.Optic.Concrete.Categories.Lens as Optic

data Forest a e = 
	Forest 
	{
		additional :: a,
		trees :: [Tree a e]
	}
	deriving (Eq, Functor, Foldable, Traversable)


data Tree a e =
	Node
	{
		trunk :: e,
		forest :: Forest a e
	}
	deriving (Eq, Functor, Foldable, Traversable)


{- --------------------- optics ------------------------- -}

additional_in_Forest :: Optic.Lens' a (Forest a e)
additional_in_Forest = Optic.lens_from_get_set additional (\ a f -> f { additional = a })

trees_in_Forest :: Optic.Lens [Tree a e1] [Tree a e2] (Forest a e1) (Forest a e2)
trees_in_Forest = Optic.lens_from_get_set trees (\ t f -> f { trees = t })

trunk_in_Tree :: Optic.Lens' e (Tree a e)
trunk_in_Tree = Optic.lens_from_get_set trunk (\ u e -> e { trunk = u })

forest_in_Tree :: Optic.Lens' (Forest a e) (Tree a e)
forest_in_Tree = Optic.lens_from_get_set forest (\ f t -> t { forest = f })
