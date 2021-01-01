module Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Test
(
	test,
)

where


import Fana.Develop.Test.Define (Test)

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Lines as Lines


test :: Test
test = Test.bunch "simco" [Lines.test]
