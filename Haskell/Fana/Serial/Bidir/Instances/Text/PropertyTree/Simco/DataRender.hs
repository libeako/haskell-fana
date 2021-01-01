module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataRender
(
	render_tree, render_forest,
)
where

import Fana.Prelude
import Data.Tree (Tree (..))
import Prelude (String)

import qualified Data.Foldable as Foldable
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as High -- high level data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as Low -- low level data


type Text = String

meaningful_common_render :: High.SemanticCommon -> Low.MeaningfulCommon
meaningful_common_render (High.SemanticCommon is_active name comments) = Low.MeaningfulCommon is_active name

render_tree :: High.Tree -> [Tree Low.Node]
render_tree =
	\case
		High.MakeSemantic mn -> 
			case mn of
				High.MakeProperty meaningful_common value -> 
					let node_data = Low.NodeMeaningful (Low.NodeProperty (meaningful_common_render meaningful_common) value)
					in [Node node_data ((map <<< map) Low.NodeComment (High.comment meaningful_common))]
				High.MakeCategory meaningful_common children ->
					let trunk = Low.NodeMeaningful (Low.NodeCategory (meaningful_common_render meaningful_common))
					in [Node trunk (render_forest children)]
		High.MakeComment comment_tree -> []

render_forest :: [High.Tree] -> [Tree Low.Node]
render_forest = map render_tree >>> Foldable.concat



