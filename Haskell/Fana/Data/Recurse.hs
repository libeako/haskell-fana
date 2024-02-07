module Fana.Data.Recurse
(
	Fix (..),
	StructureIsKnown (..), Recursive (..), CoRecursive (..),
	cata, ana, para, apo, histo, futu,
	hylo,
	cata_with_original, cata_with_path_to_root,
	change_per_node, add_path,
)
where

import Fana.Prelude.FromBase
import Fana.Haskell.Wrap

import qualified Data.Functor.Compose as Functor
import qualified Data.Tree as Base
import qualified Fana.Math.Algebra.Category.Functor.Monoidal.Monad.FreeOverFunctor as FreeOf
import qualified Fana.Math.Algebra.Category.Functor.Pro as ProFr


{-| The class of types whose underlying structure is known. -}
class StructureIsKnown (t :: Type) where
	{-| The underlying structure, without the recursion. -}
	type StructureOf t :: Type -> Type

class (StructureIsKnown r, Functor (StructureOf r)) => Recursive (r :: Type) where
	show_structure :: r -> StructureOf r r

class (StructureIsKnown r, Functor (StructureOf r)) => CoRecursive (r :: Type) where
	hide_structure :: StructureOf r r -> r

newtype Fix f = Fix { unwrapFix :: f (Fix f) }

instance Wrap (Fix f) where
	type Unwrap (Fix f) = f (Fix f)

instance StructureIsKnown (Fix f) where
	type StructureOf (Fix f) = f

instance Functor f => Recursive (Fix f) where
	show_structure = unwrapFix

instance Functor f => CoRecursive (Fix f) where
	hide_structure = Fix


instance StructureIsKnown (Base.Tree e) where
	type StructureOf (Base.Tree e) = []
instance Recursive (Base.Tree e) where
	show_structure = Base.subForest


{-| Destructing a recursive structure. -}
cata :: Recursive r => (StructureOf r e -> e) -> (r -> e)
cata step = show_structure >>> map (cata step) >>> step

{-|
	Destructing a recursive structure,
	having access to the original.
-}
cata_with_original ::
	forall r e . Recursive r =>
	(r -> StructureOf r e -> e) -> (r -> e)
cata_with_original step r =
	step r (map (cata_with_original step) (show_structure r))

{-|
	Destructing a recursive structure,
	having access to the path to root.
-}
cata_with_path_to_root' ::
	forall r e . Recursive r =>
	[r] -> ((r, [r]) -> StructureOf r e -> e) -> (r -> e)
cata_with_path_to_root' path_so_far step r =
	step (r, path_so_far) (map (cata_with_path_to_root' (r : path_so_far) step) (show_structure r))

{-|
	Destructing a recursive structure,
	having access to the path to root.
-}
cata_with_path_to_root ::
	forall r e . Recursive r =>
	((r, [r]) -> StructureOf r e -> e) -> (r -> e)
cata_with_path_to_root step r = cata_with_path_to_root' [] step r

{-| Building a corecursive structure. -}
ana :: CoRecursive r => (e -> StructureOf r e) -> (e -> r)
ana step = hide_structure <<< map (ana step) <<< step

{-|
	Destructing a recursive structure,
	having access to the original values.
-}
para :: Recursive r => (StructureOf r (r, e) -> e) -> (r -> e)
para step = show_structure >>> map (liftA2 (,) id (para step)) >>> step

{-|
	Building a corecursive structure,
	having the possibility to end the corecursion by specifying a result.
-}
apo :: CoRecursive r => (e -> StructureOf r (Either r e)) -> (e -> r)
apo step = hide_structure <<< map (either id (apo step)) <<< step

{-|
	Destructing a recursive structure,
	having access to all the lower level original values.
-}
histo' ::
	forall r e . Recursive r =>
	(StructureOf r (FreeOf.Om (StructureOf r) e) -> e) -> (r -> FreeOf.Om (StructureOf r) e)
histo' step =
	let
		core :: StructureOf r r -> FreeOf.Om (StructureOf r) e
		core = map (histo' step) >>> liftA2 FreeOf.Om id step
		in show_structure >>> core

{-|
	Destructing a recursive structure,
	having access to all the lower level original values.
-}
histo :: Recursive r => (StructureOf r (FreeOf.Om (StructureOf r) e) -> e) -> (r -> e)
histo = histo' >>> map FreeOf.extract

{-|
	Building a corecursive structure,
	having the possibility to end the corecursion by specifying results
	at any lower levels.
-}
futu' ::
	forall r e . CoRecursive r =>
	(e -> StructureOf r (FreeOf.Mo (StructureOf r) e)) -> (FreeOf.Mo (StructureOf r) e -> r)
futu' step =
	let
		layer :: FreeOf.Mo (StructureOf r) e -> StructureOf r (FreeOf.Mo (StructureOf r) e)
		layer =
			\ case
				FreeOf.Join result -> result
				FreeOf.Pure next -> step next
		in hide_structure <<< map (futu' step) <<< layer

{-|
	Building a corecursive structure,
	having the possibility to end the corecursion by specifying a results
	at any lower levels.
-}
futu :: CoRecursive r => (e -> StructureOf r (FreeOf.Mo (StructureOf r) e)) -> (e -> r)
futu = futu' >>> ProFr.map_i FreeOf.Pure


{-| Building up and then destructing a recursive structure. -}
hylo :: Functor f => (f y -> y) -> (x -> f x) -> (x -> y)
hylo y x = x >>> map (hylo y x) >>> y

{-| Change of a structure into an other one by changing nodes. -}
change_per_node ::
	(Recursive r1, CoRecursive r2) =>
	StructureOf r1 ~ StructureOf r2 =>
	(forall e . StructureOf r1 e -> StructureOf r2 e) ->
	r1 -> r2
change_per_node f = show_structure >>> map (change_per_node f) >>> f >>> hide_structure

add_path' ::
	forall r1 r2 .
	(Recursive r1, CoRecursive r2) =>
	StructureOf r2 ~ Functor.Compose ((,) [r1]) (StructureOf r1) =>
	[r1] -> r1 -> r2
add_path' path_so_far r =
	let
		attach_path :: [r1] -> StructureOf r1 e -> StructureOf r2 e
		attach_path path sr = Functor.Compose (path, sr)
		in hide_structure (attach_path path_so_far (map (add_path' (r : path_so_far)) (show_structure r)))

{-|
	Adds the path from the root to each node.
	The path of a node does not contain that node.
-}
add_path ::
	forall r1 r2 .
	(Recursive r1, CoRecursive r2) =>
	StructureOf r2 ~ Functor.Compose ((,) [r1]) (StructureOf r1) =>
	r1 -> r2
add_path = add_path' []
