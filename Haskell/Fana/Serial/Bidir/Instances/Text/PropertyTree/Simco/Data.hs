-- | Data structure of the abstract syntax of the Simco language.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data
(
	Comment, Tree (..), PropertyValue (..), Semantic (..), 
	make_simple_comment, make_atom, make_composite,
)
where

import Fana.Prelude
import Prelude (String)

import qualified Data.Tree as Base

{-|
	Data structure of a simple_comment. 
	
	Comment has tree structure too.
-}
type Comment = Base.Tree String 

-- | The recursive part of the data structure.
data Tree = MakeSemantic Semantic | MakeComment Comment
-- | Tree that contains meaningful data, that is - not comment.
data Semantic = Semantic { is_active :: Bool, name :: String, comment :: [Comment], value :: PropertyValue }
data PropertyValue = MakeAtom String | MakeComposite [Tree]


-- * Helper constructors

-- | Just a different name for the regular 'Node' constructor for a simple comment,
-- one that is consistent with other helper constructors.
make_simple_comment :: String -> Tree
make_simple_comment = flip Base.Node [] >>> MakeComment

make_atom :: String -> String -> [Comment] -> Tree
make_atom name' value' comments = MakeSemantic (Semantic True name' comments (MakeAtom value'))

make_composite :: String -> [Tree] -> [Comment] -> Tree
make_composite name' content comments = MakeSemantic (Semantic True name' comments (MakeComposite content))
