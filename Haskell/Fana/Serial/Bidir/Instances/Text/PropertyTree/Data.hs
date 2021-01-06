-- | Data structure representing tree of properties.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
(
	PropertyName, AtomicPropertyValue,
	PropertyListStructure, PropertyStructure (..),
	PropertyList, Property, NamedProperty,
)
where

import Prelude (String)


type PropertyName = String
type AtomicPropertyValue = String
type NamedProperty e = (PropertyName, PropertyStructure e)
type PropertyListStructure e = [NamedProperty e]
data PropertyStructure e = MakeAtomicProperty e | MakeCompositeProperty (PropertyListStructure e)
type PropertyList = PropertyListStructure AtomicPropertyValue
type Property = PropertyStructure AtomicPropertyValue
