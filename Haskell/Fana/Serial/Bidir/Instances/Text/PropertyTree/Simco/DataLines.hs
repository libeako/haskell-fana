-- | The data structure of a single line of Simco and related stuff.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines
(
	Name, PropertyAtomValue, Activity (..),
	Semantic (..), ActiveNode (..), NodeWithActivity (..),
	process_Node,
	make_atom,
	to_props,
)
where

import Fana.Prelude
import Prelude (String)

import qualified Data.Tree as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Recurse as Recurse
import qualified Fana.Data.Tree.Discriminating as DiscrTree
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Data as PropTree


type Name = String
type PropertyAtomValue = String
data Activity = Active | InActive   deriving Eq
data Semantic = Semantic { name :: Name, value :: Maybe String }   deriving Eq
data ActiveNode = MakeSemantic Semantic | MakeComment String   deriving Eq
process_Node :: (Semantic -> r) -> (String -> r) -> ActiveNode -> r
process_Node on_meaningful on_comment = 
	\ case
		MakeSemantic d -> on_meaningful d
		MakeComment d -> on_comment d
type NodeWithActivity = (Activity, ActiveNode)


-- * Helper constructors

-- ~ make_simple_comment :: String -> _
-- ~ make_simple_comment = _

make_atom :: String -> String -> ActiveNode
make_atom name' value' = MakeSemantic (Semantic name' (Just value'))

-- ~ make_composite :: String -> Forest -> _
-- ~ make_composite name' children = _



-- * Filtering the active nodes

filter_tree :: forall e . Base.Tree (Maybe e) -> Maybe (Base.Tree e)
filter_tree = 
	let
		step :: Base.Tree (Maybe e) -> [Maybe (Base.Tree e)] -> Maybe (Base.Tree e)
		step old_tree new_subtrees = 
			map
				(\ e -> Base.Node e (Filter.filter new_subtrees))
				(Base.rootLabel old_tree)
		in Recurse.cata_with_original step
filter_forest :: (eo -> Maybe en) -> [Base.Tree eo] -> [Base.Tree en]
filter_forest f = map (map f >>> filter_tree) >>> Filter.filter

drop_inactive :: Base.Forest NodeWithActivity -> Base.Forest ActiveNode
drop_inactive = filter_forest (\ (a, e) -> if a == Active then (Just e) else Nothing)

drop_comment :: Base.Forest ActiveNode -> Base.Forest Semantic
drop_comment = filter_forest (\ case { MakeSemantic s -> Just s; _ -> Nothing })

forest_to_prop_list :: Base.Forest Semantic -> PropTree.PropertyList
forest_to_prop_list = map tree_to_prop >>> Filter.filter

tree_to_prop :: Base.Tree Semantic -> Maybe (PropTree.NamedProperty String)
tree_to_prop (Base.Node (Semantic n value') subtrees) = 
	case forest_to_prop_list subtrees of
		[] -> map (PropTree.Single >>> Pair.after n) value'
		s -> Just (n, PropTree.Composite s)


type ActiveForest = DiscrTree.Forest [] PropertyAtomValue () Name

to_props :: Base.Forest NodeWithActivity -> PropTree.PropertyList
to_props = drop_inactive >>> drop_comment >>> forest_to_prop_list
