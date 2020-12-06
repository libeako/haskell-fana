module Fana.Meta.Test.Data.Tree.Map.KeyIsString
(
	all_tests,
)
where

import Prelude ((+))
import Fana.Prelude

import Fana.Data.Key.LensToMaybeElement
import Fana.Data.Key.Map.KeyIsString
import Fana.Develop.Test.Define (Test)

import qualified Data.Foldable as Fold
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Trav
import qualified Fana.Data.CollectionWithEmpty as Data
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Key.LensToMaybeElement as MapI
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


-- | the type of an element of a key.
type TestKeyElem = Base.Char
type TestKey = [TestKeyElem]
type TestValue = Base.Int
type TestMap = Map TestKeyElem TestValue
type TestElem = Elem TestKeyElem TestValue

usual_elems :: [TestElem]
usual_elems = [("", 0), ("hi", 1), ("hio", 3)]

is_empty_at :: TestKey -> TestMap -> Bool
is_empty_at key = Optic.down (lens_at key) >>> Maybe.isNothing

test_is_empty :: TestMap -> Bool
test_is_empty =
	let tests = Data.is_coll_empty : (map is_empty_at (map fst usual_elems))
	in Trav.sequence tests >>> Fold.and

test_empty_is_empty :: Test
test_empty_is_empty = Test.single "is_really_empty" (test_is_empty Data.empty_coll)

test_empty :: Test
test_empty =
		let
			simple_tests =
				[
				test_empty_is_empty
				]
		in
			Test.bunch "empty" simple_tests

-- | puts the given element into the given map.
put_elem :: TestElem -> TestMap -> TestMap
put_elem elem = Optic.fill (lens_at (fst elem)) (Just (snd elem))

-- | a map with population.
populated :: TestMap
populated = Fold.foldl' (flip put_elem) Data.empty_coll usual_elems

test_populated_has_the_population :: Test
test_populated_has_the_population =
	let
		test_size = Fold.length populated == Fold.length usual_elems
		-- | whether the given element is in the map.
		test_elem :: TestElem -> Bool
		test_elem (key, value) =
			let value_found = Optic.down (lens_at key) populated
			in Maybe.maybe False (== value) value_found
		test_elems = Fold.and (map test_elem usual_elems)
		test = test_size && test_elems
	in
		Test.single "populated has the population" test

-- | tests whether changing of an element value works.
test_elem_change :: Fn.Endo TestValue -> TestElem -> Bool
test_elem_change value_changer (key, value) =
	let changed_map = Optic.fn_up (lens_at key) (map value_changer) populated
	in contains_elem key (value_changer value) changed_map

test_elem_changes :: Test
test_elem_changes =
	let test = Fold.and (map (test_elem_change (+1)) usual_elems)
	in Test.single "elem changes" test

test_deletion :: Test
test_deletion =
	let
		all_deleted :: TestMap
		all_deleted = Fold.foldl' (flip (fst >>> MapI.delete_at)) populated usual_elems
	in
		Test.single "deletion" (test_is_empty all_deleted)

test_simple_retainment :: Test
test_simple_retainment =
	let
		key = "language_version"
		retained = Maybe.isJust (MapI.get_at key (MapI.set_at key 0 (Data.empty_coll :: TestMap)))
	in
		Test.single "simple retainment" retained

all_tests :: Test
all_tests =
	let
		simple_tests =
			[
			test_populated_has_the_population,
			test_simple_retainment,
			test_elem_changes,
			test_deletion
			]
	in Test.bunch "KeyIsString" (test_empty : simple_tests)
