module Fana.Serial.Print.Indent
	(
	Text, Indentation,
	fold_line_tree, fold_line_forest,
	-- * Some Indentations
	uniform_indentation,
	lasty_indentation, default_lasty_indentation,
	)
where


import Data.String (IsString)
import Fana.Math.Algebra.Monoid.Accumulate
import Fana.Prelude.FromBase
import Prelude (Int)

import qualified Data.List as List
import qualified Data.Tree as Base
import qualified Fana.Data.Tree.OfBase as Befa
import qualified Prelude as Base


type Text t = Accumulated t
type Indentation t = Int -> Text t


{-| Folds a tree of lines together. -}
fold_line_tree :: IsString t => Indentation t -> Base.Tree (Text t) -> Text t
fold_line_tree indentation =
	let
		node_to_text_tree node =
			fold [indentation (Base.fst node), Base.snd node, single "\n"]
		in
			id
			>>> Befa.with_hight
			>>> toList
			>>> map node_to_text_tree
			>>> fold

{-| Folds a forest of lines together. -}
fold_line_forest :: IsString t => Indentation t -> Base.Forest (Text t) -> Text t
fold_line_forest = fold_line_tree >>> map >>> (>>> fold)


------------- some indentations ------------------

{-|
	Indentation, in which all elements are the given one.
-}
uniform_indentation :: t -> Indentation t
uniform_indentation e level = fold (List.replicate level (single e))

{-|
	Indentation, in which the last element is special and the other elements are uniform.
-}
lasty_indentation :: t -> t -> Indentation t
lasty_indentation leaf joint level = fold (List.replicate level (single joint)) <> single leaf

default_lasty_indentation :: IsString t => Indentation t
default_lasty_indentation = lasty_indentation ("|") (" ")
