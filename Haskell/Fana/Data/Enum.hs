module Fana.Data.Enum
(
	all_values,
)
where


import Prelude (Enum (..), Bounded (..))


all_values :: (Enum v, Bounded v) => [v]
all_values = enumFromTo minBound maxBound
