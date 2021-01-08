module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Test
(
	test,
)

where


import Fana.Develop.Test.Define (Test)

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.NodeSerial as Serial


test :: Test
test = Test.bunch "simco" [Serial.test]
