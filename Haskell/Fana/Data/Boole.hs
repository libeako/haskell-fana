module Fana.Data.Boole
(
	and, or,
)
where


import qualified Data.Bool as Base
import qualified Fana.Data.Function as Fn

and :: Fn.Combiner Base.Bool
and = (Base.&&)

or :: Fn.Combiner Base.Bool
or = (Base.||)
