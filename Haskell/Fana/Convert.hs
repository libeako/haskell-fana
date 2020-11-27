module Fana.Convert
(
	ExistsConversion (..),
)
where

import Data.Kind (Type)
import Fana.Prelude.FromBase

import qualified Data.Coerce as Base


class ExistsConversion (x :: Type) (y :: Type) where
	convert :: x -> y
	default convert :: Base.Coercible x y => x -> y
	convert = Base.coerce

instance ExistsConversion t t where convert = id
