{-|
	This file defines the same tree data structure as in base [Data.Tree],
	but is more extensible, as the structure of the children is a parameter.
-}
module Fana.Data.Tree.Uniform
(
	Node (..),
	Tree (..),
	structure,
	trunk_content, children,
	assemble,
	map_children_container,
	-- * Path
	Path, TreeWithPaths, with_paths,
)
where

import Fana.Haskell.Wrap (Wrap (Unwrap))
import Fana.Prelude.FromBase
import Prelude (fmap)

import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Recurse as Recurse
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Optic.Concrete.Prelude as Optic

{-|
	* r is the type of the recursive references to the subtrees
	* s is the type of the container that holds the subtrees
-}
data Node e s r = Node { node_content :: e, subtrees :: s r }
	deriving (Functor, Foldable, Traversable)

in_node_content :: Optic.Lens e1 e2 (Node e1 s t) (Node e2 s t)
in_node_content = Optic.lens_from_get_set node_content (\ l h -> h { node_content = l})

in_node_subtrees :: Optic.Lens (s1 t1) (s2 t2) (Node e s1 t1) (Node e s2 t2)
in_node_subtrees = Optic.lens_from_get_set subtrees (\ l h -> h { subtrees = l})

{-|
	A tree containing data elements in each node.

	In its simplest version it is isomorphic to tree data structure as in base [Data.Tree],
	but is more extensible, as the structure of the children is a parameter.
-}
newtype Tree s e = Tree { unwrapTree :: Node e s (Tree s e) }

structure :: Tree s e -> (e, s (Tree s e))
structure (Tree (Node e s)) = (e, s)

instance Wrap (Tree s e) where
	type Unwrap (Tree s e) = Node e s (Tree s e)

instance Recurse.StructureIsKnown (Tree s e) where
	type StructureOf (Tree s e) = Node e s

instance Functor s => Recurse.Recursive (Tree s e) where
	show_structure = unwrapTree
instance Functor s => Recurse.CoRecursive (Tree s e) where
	hide_structure = Tree

instance Functor s => Functor (Tree s) where
	fmap f = Recurse.cata (Optic.fn_up in_node_content f >>> Tree)

foldmap_Tree :: Foldable s => Monoid m => (e -> m) -> Tree s e -> m
foldmap_Tree f (Tree (Node e s)) = f e <> foldMap (foldmap_Tree f) s
instance Foldable s => Foldable (Tree s) where foldMap = foldmap_Tree

instance Traversable s => Traversable (Tree s) where
	sequenceA (Tree (Node e s)) =
		map Tree (liftA2 Node e (traverse sequenceA s))

assemble :: e -> s (Tree s e) -> Tree s e
assemble e s = Tree (Node e s)

trunk_content :: Tree s e -> e
trunk_content = unwrapTree >>> node_content

children :: Tree s e -> s (Tree s e)
children (Tree (Node _ s)) = s

map_children_container :: (Functor so, Functor sn) => (forall r . so r -> sn r) -> Tree so e -> Tree sn e
map_children_container f = Wrap.over (Optic.fn_up in_node_subtrees (map (map_children_container f) >>> f))


-- * Paths

{-|
	Path of a node in the tree
	[by default : from the node, not including the node, to the root].
-}
type Path s e = [Tree s e]

type TreeWithPaths s e = Tree s (e, Path s e)

{-|
	Augments the content elements in each node with the path
	from the node to the root, not including that node.
-}
with_paths :: forall s e . Functor s => Tree s e -> TreeWithPaths s e
with_paths =
	let
		step :: [Tree s e] -> Node e s (TreeWithPaths s e) -> TreeWithPaths s e
		step path = Optic.fn_up in_node_content (Pair.before path) >>> Tree
		in Recurse.cata_with_path_to_root step
