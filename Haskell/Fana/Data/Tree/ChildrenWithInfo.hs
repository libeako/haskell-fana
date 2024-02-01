{-|
	Tree data structure where the children-list [the "forest"] has some additional info.
-}

module Fana.Data.Tree.ChildrenWithInfo
(
	Forest (..), Tree (..),
)
where


import Fana.Prelude


data Forest a e = 
	Forest 
	{
		additional :: a,
		the_list :: [Tree a e]
	}
	deriving (Eq, Functor, Foldable, Traversable)

data Tree a e =
	Node
	{
		trunk :: e,
		children :: Forest a e
	}
	deriving (Eq, Functor, Foldable, Traversable)
