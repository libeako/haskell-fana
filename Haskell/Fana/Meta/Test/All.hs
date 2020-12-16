module Fana.Meta.Test.All
(
	all_tests,
)
where

import Fana.Develop.Test.Define (Test)

import qualified Fana.Meta.Test.Data.Tree.Map.KeyIsString as KeyIsString
import qualified Fana.Meta.Test.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Serial.Bidir.Meta.Test.Main as Serial


all_tests :: Test
all_tests = Test.bunch "tests" [KeyIsString.all_tests, Serial.test, TreeSerial.serialization_test]
