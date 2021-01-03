-- | Data structure of the abstract syntax of the Simco language.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data
(
	ItemStructure (..), ItemsStructure (..), TreeStructure (..), ForestStructure (..),
	CommentTree, Item (..), SemanticNode (..), SemanticTree (..), Forest,
	make_simple_comment, make_atom, make_composite,
)
where

import Fana.Data.CollectionWithEmpty
import Fana.Prelude
import Prelude (String)

import qualified Data.List as List
import qualified Data.Tree as Base
import qualified Fana.Data.Tree.Discriminating as DTree


{-|
	Data structure of a simple_comment. 
	
	Comment has tree structure too.
-}
type CommentTree = Base.Tree String
data SemanticNode = SemanticNode { is_active :: Bool, name :: String }
data ItemStructure r = MakeSemantic r | MakeComment CommentTree
	deriving Functor
-- | The forest container.
newtype ItemsStructure r = ItemsStructure { deItemsStructure :: [ItemStructure r] } 
	deriving Functor
type TreeStructure e = DTree.Tree ItemsStructure String () e
type ForestStructure e = DTree.Forest ItemsStructure String () e
type SemanticTree = TreeStructure SemanticNode
type Forest = ForestStructure SemanticNode
type Item = ItemStructure SemanticTree

instance CollWithEmpty ItemsStructure where
	empty_coll = ItemsStructure []
	is_coll_empty = deItemsStructure >>> List.null


-- * Helper constructors

make_simple_comment :: String -> Item
make_simple_comment = flip Base.Node [] >>> MakeComment

make_atom :: String -> String -> Item
make_atom name' value' = MakeSemantic (DTree.leaf (SemanticNode True name') value')

make_composite :: String -> Forest -> Item
make_composite name' children = MakeSemantic (DTree.joint (SemanticNode True name') () children)
