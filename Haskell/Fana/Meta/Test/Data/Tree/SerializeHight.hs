module Fana.Meta.Test.Data.Tree.SerializeHight
(
	serialization_test,
)

where

import Data.Tree (Tree (..))
import Fana.Data.Tree.SerializeHight
import Fana.Develop.Test.Define (Test)

import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic


serialization_test :: Test
serialization_test =
	let
		invalid_pictures =
			let
				he :: Hight -> (Hight, ())
				he = Pair.before ()
				in
					[
					  [he 1]
					, [he 0, he 2]
					]
		datas =
			let
				e :: ()
				e = ()
				in
					[
					  []
					, [Node e []]
					, [Node e [Node e []]]
					, [Node e [Node e []], Node e []]
					]
		asserted =
			Optic.test_piso
				(Category2.empty, Category2.empty)
				invalid_pictures datas
					(serializer :: Optic.PartialIso' (HightListParseError ()) [(Hight, ())] [Tree ()])
		in Test.single "tree from elem list" asserted
