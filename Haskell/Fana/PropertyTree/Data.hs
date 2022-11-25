-- | Data structure representing tree of properties.
module Fana.PropertyTree.Data
(
	PropertyName, AtomicPropertyValue,
	PropertyS, PropertyValueS (..),
	Property, PropertyList, PropertyValue,
)
where

import Prelude (String)


type PropertyName = String
type AtomicPropertyValue = String

{- "S" in the following names stands for "structure" -}

type PropertyS e = (PropertyName, PropertyValueS e)
data PropertyValueS e = PropertyValueAtomic e | PropertyValueComposite [PropertyS e]

type Property = PropertyS AtomicPropertyValue
type PropertyList = [PropertyS AtomicPropertyValue]
type PropertyValue = PropertyValueS AtomicPropertyValue
