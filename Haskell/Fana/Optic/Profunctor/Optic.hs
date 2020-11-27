{-|
	Profunctor representation of optics.

	This implementation of profunctor optics is not finished.
	I created it as a personal experiment. Do NOT use it.

	I do not see much practical use for profunctor optics.
	Hence i do not plan to finish this profunctor implementation.
	Perhaps i will even delete it from here.

	The representation is not categorized,
	the same type is used for all categories in the profunctor representation.
	I think this is the spirit of the profunctor representation : convertability between the categories.
-}

module Fana.Optic.Profunctor.Optic
(
	OpticBone, OpticNude, TypeKeeper, OpticWrapped (..),
)
where

import Data.Kind (Type, Constraint)


-- | The bone of the optic type.
--
-- - "tf" is short of "type function".
-- - "w" is short of "whole".
-- - "p" is short of "part".
type OpticBone tf p1 p2 w1 w2 = tf p1 p2 -> tf w1 w2
type OpticNude (c :: (Type -> Type -> Type) -> Constraint) p1 p2 w1 w2 =
	forall tf . c tf => OpticBone tf p1 p2 w1 w2

-- | Changes the input types of a given optic type to keep the type of data that the optic works on.
type TypeKeeper o p w = o p p w w

newtype OpticWrapped (c :: (Type -> Type -> Type) -> Constraint) p1 p2 w1 w2 =
	Wrap { unwrapOptic :: OpticNude c p1 p2 w1 w2 }
