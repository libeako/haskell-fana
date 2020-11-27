module Fana.Data.Maybe
(
	keep_iff,
)
where

import qualified Data.Bool as Base
import qualified Data.Maybe as Base


{-| The output is nothing unless the condition is true. -}
keep_iff :: (x -> Base.Bool) -> x -> Base.Maybe x
keep_iff condition x =
	if condition x
		then Base.Just x
		else Base.Nothing

