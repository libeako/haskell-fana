module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataRender
(
	render,
)
where

import Fana.Prelude
import Data.Tree (Tree (..))
import Prelude (String)

import qualified Data.Tree as Base
import qualified Fana.Data.Tree.Discriminating as DTree
import qualified Fana.Data.Tree.Uniform as UTree
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as High -- high level data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as Low -- low level data


type Text = String

render :: High.Forest -> [Tree Low.Node]
render =
	map 
		(
			DTree.homogenize
				(\ (High.SemanticNode is_active name) value -> Low.Semantic is_active name (Just value))
				(\ (High.SemanticNode is_active name) () -> Low.Semantic is_active name Nothing)
		)
	>>> render_forest

render_forest :: High.ItemsStructure ((UTree.Tree High.ItemsStructure Low.Semantic)) -> [Tree Low.Node]
render_forest = High.deItemsStructure >>> map render_item

render_item :: 
	High.ItemStructure (UTree.Tree High.ItemsStructure Low.Semantic) -> 
	Tree Low.Node
render_item =
	\ case
		High.MakeSemantic t -> render_tree t
		High.MakeComment t -> map Low.MakeComment t

render_tree :: UTree.Tree High.ItemsStructure Low.Semantic -> Tree Low.Node
render_tree (UTree.Tree (UTree.Node e children)) = Base.Node (Low.MakeSemantic e) (render_forest children)
