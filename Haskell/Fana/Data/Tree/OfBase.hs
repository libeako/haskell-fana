-- | Extensions to container base/Data.Tree.
module Fana.Data.Tree.OfBase
(
	Count, Hight,
	trunk_in_tree, children_in_tree,
	filter_shallowly, filter_deeply,
	with_hight,
	with_path_to_trunk,
)
where


import Data.Tree (Tree (..), Forest)
import Fana.Prelude.FromBase
import Prelude ((+))

import qualified Prelude as Base
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Optic.Concrete.Categories.Lens as Optic


type Count = Base.Int
type Hight = Count


trunk_in_tree :: Optic.Lens' e (Tree e)
trunk_in_tree = Optic.lens_from_get_set rootLabel (\ e t -> Node e (subForest t))

children_in_tree :: Optic.Lens' (Forest e) (Tree e)
children_in_tree = Optic.lens_from_get_set subForest (\ ch t -> Node (rootLabel t) ch)

-- | Drops Nothing nodes. But not their descendants.
filter_shallowly :: Tree (Maybe e) -> Forest e
filter_shallowly input =
	let children = (map filter_shallowly >>> Base.concat) (subForest input)
		in
			case (rootLabel input) of
				Nothing -> children
				Just e -> [Node e children]

-- | Drops non-complying nodes, including all their descendants.
filter_deeply :: Tree (Maybe x) -> Maybe (Tree x)
filter_deeply input =
	case (rootLabel input) of
		Nothing -> Nothing
		Just elem ->
			let new_children = Base.catMaybes (List.map filter_deeply (subForest input))
				in Just (Node { rootLabel = elem, subForest = new_children })


with_path_to_trunk' :: [Tree e] -> Tree e -> Tree ([Tree e], e)
with_path_to_trunk' path tree =
	let new_path = tree : path
		in Node (new_path, rootLabel tree) (Base.map (with_path_to_trunk' new_path) (subForest tree))

with_path_to_trunk :: Tree e -> Tree ([Tree e], e)
with_path_to_trunk = with_path_to_trunk' []

with_hight' :: Hight -> Tree e -> Tree (Hight, e)
with_hight' hight tree =
	let children = Base.fmap (with_hight' (hight + 1)) (subForest tree)
		in Node (hight, rootLabel tree) children

with_hight :: Tree e -> Tree (Hight, e)
with_hight = with_hight' 1
