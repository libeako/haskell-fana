module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines
(
	SemanticCommon (..), Semantic (..), Node (..),
	process_Node,
	delete_not_active_from_forest,
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


type Text = String
type Name = Text
type PropertyAtomValue = Text

data SemanticCommon = SemanticCommon { is_active :: Bool, name :: Name } 
	deriving Eq
data Semantic = Atom SemanticCommon PropertyAtomValue | Composite SemanticCommon
	deriving Eq
data Node = MakeSemantic Semantic | MakeComment Text
	deriving Eq

process_Node :: (Semantic -> r) -> (Text -> r) -> Node -> r
process_Node on_meaningful on_comment = 
	\ case
		MakeSemantic d -> on_meaningful d
		MakeComment d -> on_comment d


-- * filtering the active nodes :

type ActiveForest = [DiscrTree.Tree [] PropertyAtomValue () Name]

delete_not_active_from_tree :: Base.Tree Node -> ActiveForest
delete_not_active_from_tree (Base.Node trunk children) =
	case trunk of
		MakeComment _ -> []
		MakeSemantic meaningful -> 
			let
				answer :: 
					SemanticCommon -> 
					DiscrTree.Discrimination [] PropertyAtomValue () 
						(FanaTree.Tree (DiscrTree.Discrimination [] PropertyAtomValue ()) Name) -> 
					ActiveForest
				answer (SemanticCommon is_active' name') node_specific_part = 
					if is_active' then [FanaTree.assemble name' node_specific_part] else []
				in
					case meaningful of
						Atom common value -> answer common (DiscrTree.Leaf value)
						Composite common -> answer common (DiscrTree.Joint () (delete_not_active_from_forest children))

delete_not_active_from_forest :: Base.Forest Node -> ActiveForest
delete_not_active_from_forest = map delete_not_active_from_tree >>> Base.concat


-- * mapping :

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
forest_to_map = delete_not_active_from_forest >>> active_forest_to_map
