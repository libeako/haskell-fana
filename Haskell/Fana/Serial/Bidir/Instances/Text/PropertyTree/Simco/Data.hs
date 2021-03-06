-- | The data structure of the Simco language.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data
(
	Activity (..), CommentValue,
	ImportantNode (..), ActiveNode (..), NodeWithActivity (..),
	process_ActiveNode,
	make_atom,
	clean,
)
where

import Fana.Prelude
import Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
import Prelude (String)

import qualified Data.Tree as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Recurse as Recurse


{-| Whether a node is active. -}
data Activity = Active | InActive   deriving Eq
type CommentValue = String
data ImportantNode = ImportantNode { name :: PropertyName, value :: Maybe AtomicPropertyValue }   deriving Eq
data ActiveNode = MakeImportant ImportantNode | MakeComment CommentValue   deriving Eq
process_ActiveNode :: (ImportantNode -> r) -> (CommentValue -> r) -> ActiveNode -> r
process_ActiveNode on_meaningful on_comment = 
	\ case
		MakeImportant d -> on_meaningful d
		MakeComment d -> on_comment d
type NodeWithActivity = (Activity, ActiveNode)


-- * Helper constructors

-- ~ make_comment :: CommentValue -> _
-- ~ make_comment = _

make_atom :: String -> String -> ActiveNode
make_atom name' value' = MakeImportant (ImportantNode name' (Just value'))

-- ~ make_composite :: PropertyName -> Forest -> _
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

drop_comment :: Base.Forest ActiveNode -> Base.Forest ImportantNode
drop_comment = filter_forest (\ case { MakeImportant s -> Just s; _ -> Nothing })

forest_to_prop_list :: Base.Forest ImportantNode -> PropertyList
forest_to_prop_list = map tree_to_prop >>> Filter.filter

tree_to_prop :: Base.Tree ImportantNode -> Maybe (NamedProperty String)
tree_to_prop (Base.Node (ImportantNode n value') subtrees) = 
	case forest_to_prop_list subtrees of
		[] -> map (MakeAtomicProperty >>> Pair.after n) value'
		s -> Just (n, MakeCompositeProperty s)


clean :: Base.Forest NodeWithActivity -> PropertyList
clean = drop_inactive >>> drop_comment >>> forest_to_prop_list
