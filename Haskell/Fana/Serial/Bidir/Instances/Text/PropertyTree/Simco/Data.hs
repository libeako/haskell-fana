-- | Data structure of the abstract syntax of the Simco language.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data
(
	Comment, Tree (..), SemanticCommon (..), Semantic (..), 
	make_simple_comment, make_atom, make_composite,
)
where

import Fana.Prelude
import Prelude (String)

import qualified Data.Tree as Base

{-|
	Data structure of a simple_comment. 
	
	Itself has tree structure too.
-}
type Comment = Base.Tree String 

-- | The recursive part of the data structure.
data Tree = MakeSemantic Semantic | MakeComment Comment
-- | The common part of semantic nodes.
data SemanticCommon = SemanticCommon { is_active :: Bool, name :: String, comment :: [Comment] }
-- | Tree that contains meaningful data, that is - not comment.
data Semantic = MakeAtom SemanticCommon String | MakeComposite SemanticCommon [Tree]


-- * Helper constructors

-- | Just a different name for the regular 'Node' constructor for a simple comment,
-- one that is consistent with other helper constructors.
make_simple_comment :: String -> Tree
make_simple_comment = flip Base.Node [] >>> MakeComment

make_atom :: String -> String -> [Comment] -> Tree
make_atom name' value comments = MakeSemantic (MakeAtom (SemanticCommon True name' comments) value)

make_composite :: String -> [Tree] -> [Comment] -> Tree
make_composite name' content comments = MakeSemantic (MakeComposite (SemanticCommon True name' comments) content)
