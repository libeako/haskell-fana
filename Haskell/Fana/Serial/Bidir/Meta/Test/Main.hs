module Fana.Serial.Bidir.Meta.Test.Main
(
	test,
)
where

import Fana.Develop.Test.Define (Test)

import qualified Fana.Serial.Bidir.Meta.Test.Instances as Instances

test :: Test
test = Instances.test


