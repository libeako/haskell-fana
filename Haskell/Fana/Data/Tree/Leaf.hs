-- | Tree containing elements only in leafs.
module Fana.Data.Tree.Leaf
(
	Node (..),
	Tree (..),
	leaf, joint,
)
where

import Control.Monad
import Fana.Haskell.Wrap
import Fana.Math.Algebra.Category.Functor.Monoidal.Monad.MethodFromMethod
	(applicative_apply_from_monad_join, monad_apply_from_monad_join)
import Fana.Prelude
import Prelude hiding (map, concat)

import qualified Fana.Data.Recurse as Recurse


{-|
	One node of a tree.

	* 'b' [as "branching"] is the type of the container holding the child branches.
	* 'r' is the type of the recursive subtree references.
-}
data Node b e r = Leaf e | Joint (b r)
	deriving (Functor, Foldable, Traversable)

{-|
	Tree containing elements only in leafs.

	* 'b' [as "branching"] is the type of the container holding the child branches.
-}
newtype Tree b e = Tree { unwrapTree :: Node b e (Tree b e) }

instance Wrap (Tree b e) where
	type Unwrap (Tree b e) = Node b e (Tree b e)

instance Recurse.StructureIsKnown (Tree b e) where
	type StructureOf (Tree b e) = Node b e

instance Functor b => Recurse.Recursive (Tree b e) where
	show_structure = unwrapTree

instance Functor b => Recurse.CoRecursive (Tree b e) where
	hide_structure = Tree


leaf :: e -> Tree b e
leaf = Tree <<< Leaf

joint :: b (Tree b e) -> Tree b e
joint = Tree <<< Joint


instance Functor b => Functor (Tree b) where
	fmap f (Tree s) =
		case s of
			Leaf e -> leaf (f e)
			Joint cs -> joint (map (map f) cs)
instance Foldable b => Foldable (Tree b) where
	foldMap f (Tree s) =
		 case s of
			Leaf e -> f e
			Joint cs -> foldMap (foldMap f) cs
instance Traversable b => Traversable (Tree b) where
	sequenceA (Tree s) =
		case s of
			Leaf e -> map leaf e
			Joint cs -> map joint (traverse sequenceA cs)


-- * monad

monad_join :: Functor b => Tree b (Tree b e) -> Tree b e
monad_join (Tree s) =
	case s of
		Leaf x -> x
		Joint subtrees -> joint (map monad_join subtrees)

instance Functor b => Applicative (Tree b) where
	pure = Leaf >>> Tree
	(<*>) = applicative_apply_from_monad_join (monad_join)

instance Functor b => Monad (Tree b) where
	return = pure
	(>>=) = monad_apply_from_monad_join monad_join
