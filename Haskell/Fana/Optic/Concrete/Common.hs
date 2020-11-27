module Fana.Optic.Concrete.Common
(
	Optic, Simple, Empty,
)
where

import Data.Kind (Type)

type Optic = Type -> Type -> Type -> Type -> Type

type Simple o e c = o e e c c
type Empty o t1 t2 = o t1 t2 t1 t2


