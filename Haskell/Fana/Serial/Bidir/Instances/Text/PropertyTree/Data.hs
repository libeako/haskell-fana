-- | Data structure representing tree of properties.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
(
	PropertyListStructure, PropertyStructure (..),
	PropertyList, Property,
)
where

import Prelude (String)


type PropertyListStructure e = [(String, PropertyStructure e)]
data PropertyStructure e = Single e | Composite (PropertyListStructure e)

type PropertyList = PropertyListStructure String
type Property = PropertyStructure String
