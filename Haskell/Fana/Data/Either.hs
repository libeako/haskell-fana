module Fana.Data.Either
(
	swap,
	upgrade_Maybe, refine_Maybe, from_Maybe,
)
where

import Fana.Prelude.FromBase

import qualified Data.Either as Base
import qualified Data.Maybe as Base


swap :: Either a b -> Either b a
swap = Base.either Right Left

upgrade_Maybe :: Maybe e -> Either () e
upgrade_Maybe = maybe (Left ()) Right

refine_Maybe :: error -> Maybe elem -> Either error elem
refine_Maybe e = Base.maybe (Left e) Right

from_Maybe :: error -> (e1 -> e2) -> Maybe e1 -> Either error e2
from_Maybe er f = Base.maybe (Left er) (f >>> Right)

