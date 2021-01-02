-- | Tree with different joint and leaf data type.
module Fana.Data.Tree.Discriminating
(
	Tree, Forest, Discrimination (..),
	assemble, leaf, joint,
	-- * Map
	map_all, map_to_homo,
	-- * Filter
	commons, leafs,
	-- * Else
	copy_common_to_discriminated,
)
where


import Control.Category as Category hiding (id)
import Fana.Prelude

import qualified Data.Tree as Homo
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Recurse as Recurse
import qualified Fana.Data.Tree.Leaf as Leafy
import qualified Fana.Data.Tree.Uniform as Uniform
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Optic.Concrete.Categories.Prism as Prism
import qualified Fana.Optic.Profunctor.Categories as OpticP
import qualified Fana.Optic.Profunctor.Interface as OpticP


{-|
	Part of a tree node that is specific to the type of node.

	* 'b' [as "branching"] is the type of the container holding the child branches.
	* 'r' is the type of the recursive tree references.
-}
data Discrimination b l j r =
	  Leaf l
	| Joint j (b r)
	deriving (Functor, Foldable, Traversable)

type DiscriminationType b l j e = Discrimination b l j (Tree b l j e)

prism_Joint_in_Discrimination ::
	forall b l j1 j2 t1 t2 .
		OpticP.Prism
			(j1,  b t1) (j2, b t2)
			(Discrimination b l j1 t1) (Discrimination b l j2 t2)
prism_Joint_in_Discrimination = OpticP.from_adapted (Prism.Prism match (uncurry Joint))
	where
		match ::
			Discrimination b l j1 t1 ->
			Either (Discrimination b l j2 t2) (j1, b t1)
		match = \ case { Leaf l -> Left (Leaf l); Joint j cs -> Right (j, cs) }

type Node b l j e r = Uniform.Node e (Discrimination b l j) r

subtrees_in_Discrimination :: Foldable b => Discrimination b l j r -> [r]
subtrees_in_Discrimination =
	\ case
		Leaf _ -> []
		Joint _ cs -> toList cs

{-|
	Tree containing elements
	of type 'l' in leafs, of type 'j' in joints [in container type 'b'], of type 'e' in all kinds of nodes.
-}
type Tree b l j e = Uniform.Tree (Discrimination b l j) e
type Forest b l j e = b (Tree b l j e)

type NodeType b l j e = Node b l j e (Tree b l j e)

assemble :: Discrimination b l j (Tree b l j e) -> e -> Tree b l j e
assemble d c = Uniform.Tree (Uniform.Node c d)

leaf :: e -> l -> Tree b l j e
leaf e l = Uniform.Tree (Uniform.Node e (Leaf l))

joint :: e -> j -> b (Tree b l j e) -> Tree b l j e
joint e j children = Uniform.Tree (Uniform.Node e (Joint j children))


{-| Strips the tree of all the discriminated data. -}
commons :: forall b l j e . (Functor b, Foldable b) => Tree b l j e -> Homo.Tree e
commons =
	let
		algebra :: Node b l j e (Homo.Tree e) -> Homo.Tree e
		algebra (Uniform.Node e d) = Homo.Node e (subtrees_in_Discrimination d)
		in Recurse.cata algebra

leafs :: Functor b => Tree b l j e -> Leafy.Tree b l
leafs =
	Uniform.unwrapTree >>> Uniform.subtrees >>>
	\ case
		Leaf l -> Leafy.leaf l
		Joint _ children -> Leafy.joint (map leafs children)


-- * map

map_all ::
	forall b il ij ie ol oj oe .
	Functor b =>
	((ie, il) -> (oe, ol)) ->
	((ie, ij) -> (oe, oj)) ->
	Tree b il ij ie ->
	Tree b ol oj oe
map_all fl fj =
	let
		algebra :: Node b il ij ie (Tree b ol oj oe) -> Tree b ol oj oe
		algebra (Uniform.Node ie d) =
			case d of
				Leaf il -> let (oe, ol) = fl (ie, il) in leaf oe ol
				Joint ij cs -> let (oe, oj) = fj (ie, ij) in joint oe oj cs
		in Recurse.cata algebra

{-| Mapp to a homogeneous tree. -}
map_to_homo ::
	forall o b il ij ie .
	(Functor b, Foldable b) =>
	(ie -> il -> o) -> (ie -> ij -> o) ->
	Tree b il ij ie -> Homo.Tree o
map_to_homo fl fj =
	let
		change_fn :: (x -> y -> r) -> ((x, y) -> (r, ()))
		change_fn = uncurry >>> map (Pair.before ())
		decorate = Pair.before ()
		in map_all (change_fn fl) (change_fn fj) >>> commons


-- * copy common data to descriminated data

copy_common_to_discriminated_in_Discrimination ::
	Functor b =>
	e -> Discrimination b l j (Tree b l j e) -> DiscriminationType b (e, l) (e, j) e
copy_common_to_discriminated_in_Discrimination c =
	\ case
		Leaf l -> Leaf (c, l)
		Joint j children -> Joint (c, j) (map copy_common_to_discriminated children)

copy_common_to_discriminated_in_Node ::
	Functor b =>
	NodeType b l j e -> NodeType b (e, l) (e, j) e
copy_common_to_discriminated_in_Node
	(Uniform.Node common_data discrimination) =
	Uniform.Node
		common_data
		(copy_common_to_discriminated_in_Discrimination common_data discrimination)

{-|
	Copies the data present in all nodes to the disciminated data.
-}
copy_common_to_discriminated :: Functor b => Tree b l j e -> Tree b (e, l) (e, j) e
copy_common_to_discriminated = Wrap.over copy_common_to_discriminated_in_Node
