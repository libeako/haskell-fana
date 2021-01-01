module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataRender
(
	render,
)
where

import Fana.Prelude
import Data.Tree (Tree (..))
import Prelude (String)

import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as High -- high level data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as Low -- low level data


type Text = String

render :: High.Tree -> Tree Low.Node
render =
	\ case
		High.MakeSemantic (High.Semantic is_active name comment value) -> 
			case value of
				High.MakeAtom text -> 
					let node_data = Low.MakeSemantic (Low.Semantic is_active name (Just text))
						in (Node node_data ((map <<< map) Low.MakeComment comment))
				High.MakeComposite children ->
					let trunk = Low.MakeSemantic (Low.Semantic is_active name Nothing)
						in Node trunk (map render children)
		High.MakeComment comments -> map Low.MakeComment comments

