module Main where

import qualified System.IO as Sys

import Fana.Develop.Test.Define (Test (..))
import Fana.Prelude

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Develop.Test.Run as Test
import qualified Fana.Meta.Test.All as Tests

fail :: Test
fail = Test.single "dummy test" False

main :: Sys.IO ()
main = Test.run Tests.all_tests
