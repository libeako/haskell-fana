-- | Data structure representing tree of properties.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
(
	PropertyListStructure, PropertyStructure (..),
	PropertyList, Property, NamedProperty,
)
where

import Prelude (String)


type NamedProperty e = (String, PropertyStructure e)
type PropertyListStructure e = [NamedProperty e]
data PropertyStructure e = Single e | Composite (PropertyListStructure e)

type PropertyList = PropertyListStructure String
type Property = PropertyStructure String
