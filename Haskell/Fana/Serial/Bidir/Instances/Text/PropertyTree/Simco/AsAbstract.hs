module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.AsAbstract
(
	render, parse, serialize,
)
where

import Fana.Prelude
import Fana.PropertyTree.Data
import Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data

import qualified Data.Tree as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Optic.Concrete.Prelude as Optic


forest_to_prop_list :: Base.Forest ImportantNode -> PropertyList
forest_to_prop_list = map tree_to_prop >>> Filter.filter

tree_to_prop :: Base.Tree ImportantNode -> Maybe Property
tree_to_prop (Base.Node (ImportantNode n value') subtrees) = 
	case forest_to_prop_list subtrees of
		[] -> map (PropertyValueAtomic >>> Pair.after n) value'
		s -> Just (n, PropertyValueComposite s)


render_property :: Property -> Base.Tree NodeWithActivity
render_property (n, v) = 
	case v of
		PropertyValueAtomic av -> Base.Node (Active, MakeImportant (ImportantNode n (Just av))) []
		PropertyValueComposite cv -> 
			Base.Node (Active, MakeImportant (ImportantNode n Nothing))
				(map render_property cv)

render :: PropertyList -> Base.Forest NodeWithActivity
render = map render_property

parse :: Base.Forest NodeWithActivity -> PropertyList
parse = drop_inactive >>> drop_comment >>> forest_to_prop_list

serialize :: Optic.Iso' (Base.Forest NodeWithActivity) PropertyList
serialize = Optic.Iso render parse
