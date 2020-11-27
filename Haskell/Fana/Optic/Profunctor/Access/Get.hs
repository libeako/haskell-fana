module Fana.Optic.Profunctor.Access.Get
(
	get, match,
)
where

import Fana.Math.Algebra.Category.Functor.Pro (Profunctor)
import Fana.Optic.Profunctor.Optic
import Fana.Prelude
import Prelude (snd)

import qualified Data.Bifunctor as BiFr
import qualified Data.Either as Either
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Optic.Concrete.Categories.Interfaces as Concrete
import qualified Fana.Optic.Concrete.Categories.Iso as Concrete


newtype Forget  r a b = Forget  { deForget  :: (a ->          r) }
newtype ForgetE r a b = ForgetE { deForgetE :: (a -> Either b r) }

iso_Forget ::
	Concrete.Iso
		(a1 -> r1)
		(a2 -> r2)
		(Forget r1 a1 b1)
		(Forget r2 a2 b2)
iso_Forget = Concrete.Iso deForget Forget
iso_ForgetE ::
	Concrete.Iso
		(a1 -> Either b1 r1)
		(a2 -> Either b2 r2)
		(ForgetE r1 a1 b1)
		(ForgetE r2 a2 b2)
iso_ForgetE = Concrete.Iso deForgetE ForgetE

instance Profunctor (Forget  r) where
	dimap f _ = Concrete.fn_up iso_Forget (f >>>)
instance Profunctor (ForgetE r) where
	dimap f g = Concrete.fn_up iso_ForgetE ((f >>>) >>> (>>> BiFr.first g))

instance Profunctor.LoadableP (Forget r) where
	load_pe = Concrete.fn_up iso_Forget (snd >>>)
instance Profunctor.LoadableP (ForgetE r) where
	load_pe =
		let
			move_in :: (c, Either b r) -> Either (c, b) r
			move_in (c, ei) = BiFr.first (HePair.after c) ei
		in
			Concrete.fn_up iso_ForgetE (Profunctor.load_pe >>> (>>> move_in))
instance Profunctor.LoadableS (ForgetE r) where
	load_se =
		let
			move_in :: (Either c (Either b r)) -> Either (Either c b) r
			move_in = Either.either (Left >>> Left) (BiFr.first Right)
		in
			Concrete.fn_up iso_ForgetE (Profunctor.load_se >>> (>>> move_in))
instance Profunctor.LoadablePS (ForgetE r) where


get :: OpticBone (Forget p) p p' w w' -> w -> p
get o w = deForget (o (Forget id)) w

match :: OpticBone (ForgetE p) p p' w w' -> w -> Either w' p
match o w = deForgetE (o (ForgetE Right)) w
