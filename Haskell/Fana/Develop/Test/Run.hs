module Fana.Develop.Test.Run
(
	run,
)
where


import Prelude (String)
import Fana.Prelude
import Fana.Develop.Test.Define

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Prelude as Base
import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified Fana.Data.Tree.Uniform as Tree
import qualified Fana.Data.Tree.Discriminating as DTree


data TestWithPaths = TestWithPaths { tpResult :: Bool, tpNamePath :: [Text] }

get_test_with_path :: (Name, PassValue, [DTree.Tree [] PassValue () Name]) -> TestWithPaths
get_test_with_path (name, pass_value, path) =
	TestWithPaths pass_value (map Tree.trunk_content (List.reverse (path)))

get_tests_with_paths :: Test -> [TestWithPaths]
get_tests_with_paths =
	let
		convert ::
			((Name, Tree.Path (DTree.Discrimination [] PassValue ()) Name), PassValue) ->
			(Name, PassValue, [DTree.Tree [] PassValue () Name])
		convert ((name, path), pv) = (name, pv, path)
		in
			id >>>
				Tree.with_paths True >>> DTree.copy_common_to_discriminated >>> DTree.leafs >>> Fold.toList >>>
				map (convert >>> get_test_with_path)

path_of_error :: Test -> Maybe [Text]
path_of_error = get_tests_with_paths >>> Fold.find (tpResult >>> Base.not) >>> map tpNamePath

process_reaction :: (String, Sys.ExitCode) -> Base.IO ()
process_reaction (message, ec) = Sys.putStrLn message *> Sys.exitWith ec

action_on_error_path :: Maybe [Text] -> Sys.IO ()
action_on_error_path =
	let
		react :: Maybe [Text] -> (String, Sys.ExitCode)
		react = \ case
			Nothing -> ("all tests passed", Sys.ExitSuccess)
			Just path ->
				let message = "Test failed: " <> (List.intercalate ": " path) <> "."
					in (message, Sys.ExitFailure 1)
		in react >>> process_reaction

run :: Test -> Sys.IO ()
run = path_of_error >>> action_on_error_path
