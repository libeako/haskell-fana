module Fana.Data.FunctionUpIo
(
	UpI (..),
	UpO (..),
	iso_UpI,
	iso_UpO,
)
where



import Fana.Math.Algebra.Category.Functor.Pro (Profunctor (..))
import Fana.Prelude

import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Optic.Concrete.Categories.Interfaces as Optic
import qualified Fana.Optic.Concrete.Categories.Iso as Optic


-- | Ups the input type of a function type.
newtype UpI f i o = UpI { unwrapUpI :: f i -> o }
-- | Ups the output type of a function type.
newtype UpO f i o = UpO { unwrapUpO :: i -> f o }

instance Functor f => Profunctor (UpI f) where
	dimap fi fo (UpI f) = UpI (map fi >>> f >>> fo)
instance Functor f => Profunctor (UpO f) where
	dimap fi fo (UpO f) = UpO (fi >>> f >>> map fo)

iso_UpI :: Optic.Iso (f i1 -> o1) (f i2 -> o2) (UpI f i1 o1) (UpI f i2 o2)
iso_UpO :: Optic.Iso (i1 -> f o1) (i2 -> f o2) (UpO f i1 o1) (UpO f i2 o2)
iso_UpI = Optic.Iso unwrapUpI UpI
iso_UpO = Optic.Iso unwrapUpO UpO

load_pe_ :: Functor f => (i -> f o) -> ((c, i) -> f (c, o))
load_pl_ :: Functor f => (i -> f o) -> ((i, c) -> f (o, c))
load_pe_ f (c, i) = map (HePair.after   c) (f i)
load_pl_ f (i, c) = map (HePair.before  c) (f i)

load_se_ :: Applicative f => (i -> f o) -> (Either c i -> f (Either c o))
load_sl_ :: Applicative f => (i -> f o) -> (Either i c -> f (Either o c))
load_se_ f = either (Left >>> pure) (f >>> map Right)
load_sl_ f = either (f >>> map Left) (Right >>> pure)

instance Functor f => Profunctor.LoadableP (UpO f) where
	load_pe = Optic.fn_up iso_UpO load_pe_
	load_pl = Optic.fn_up iso_UpO load_pl_

instance Applicative f => Profunctor.LoadableS (UpO f) where
	load_se = Optic.fn_up iso_UpO load_se_
	load_sl = Optic.fn_up iso_UpO load_sl_

instance Applicative f => Profunctor.LoadablePS (UpO f) where
