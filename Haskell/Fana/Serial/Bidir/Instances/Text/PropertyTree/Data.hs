-- | Data structure representing tree of properties.
module Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
(
	PropertyName, AtomicPropertyValue,
	PropertyS, PropertyValueS (..),
	NamedProperty, PropertyList, Property,
)
where

import Prelude (String)


type PropertyName = String
type AtomicPropertyValue = String

{- "S" in the following names stands for "structure" -}

type PropertyS e = (PropertyName, PropertyValueS e)
data PropertyValueS e = PropertyValueAtomic e | PropertyValueComposite [PropertyS e]

type NamedProperty = PropertyS AtomicPropertyValue
type PropertyList = [PropertyS AtomicPropertyValue]
type Property = PropertyValueS AtomicPropertyValue
