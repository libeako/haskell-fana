-- | The data structure of a single line of Simco and related stuff.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines
(
	Name, PropertyAtomValue,
	Semantic (..), Node (..),
	process_Node,
	forest_to_map,
)
where

import Fana.Prelude
import Prelude (String)

import qualified Data.Foldable as Base
import qualified Data.Tree as Base
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Tree.Uniform as FanaTree
import qualified Fana.Data.Tree.Discriminating as DiscrTree
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Data as PropTree


type Name = String
type PropertyAtomValue = String

data Semantic = Semantic { is_active :: Bool, name :: Name, value :: Maybe String }
	deriving Eq
data Node = MakeSemantic Semantic | MakeComment String
	deriving Eq

process_Node :: (Semantic -> r) -> (String -> r) -> Node -> r
process_Node on_meaningful on_comment = 
	\ case
		MakeSemantic d -> on_meaningful d
		MakeComment d -> on_comment d


-- * Filtering the active nodes

type ActiveForest = [DiscrTree.Tree [] PropertyAtomValue () Name]

keep_active_from_tree :: Base.Tree Node -> ActiveForest
keep_active_from_tree (Base.Node trunk children) =
	case trunk of
		MakeComment _ -> []
		MakeSemantic (Semantic False _ _) -> []
		MakeSemantic (Semantic _ name' value') ->
			let
				node_specific_part = maybe (DiscrTree.Joint () (keep_active_from_forest children)) DiscrTree.Leaf value'
				in [FanaTree.assemble name' node_specific_part]

keep_active_from_forest :: Base.Forest Node -> ActiveForest
keep_active_from_forest = map keep_active_from_tree >>> Base.concat


-- * Mapping

from_tree_to_key_value_at :: 
	[Name] -> DiscrTree.Tree [] PropertyAtomValue () Name -> (Name, PropTree.Property)
from_tree_to_key_value_at path = 
	FanaTree.structure >>> 
	\ case 
		(name', node_specific) ->
			case node_specific of
				DiscrTree.Leaf property_value -> (name', PropTree.Single property_value)
				DiscrTree.Joint _ children -> 
					(PropTree.Composite >>> Pair.after name') (forest_to_map_at (name' : path) children)

forest_to_map_at :: [Name] -> ActiveForest -> PropTree.PropertyList
forest_to_map_at path = map (from_tree_to_key_value_at path)

active_forest_to_map :: ActiveForest -> PropTree.PropertyList
active_forest_to_map = forest_to_map_at []

forest_to_map :: Base.Forest Node -> PropTree.PropertyList
forest_to_map = keep_active_from_forest >>> active_forest_to_map
