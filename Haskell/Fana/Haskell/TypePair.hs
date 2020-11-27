module Fana.Haskell.TypePair
(
	TypePair,
	Fst, Snd,
	-- * Unite and divorce TypePair inputs
	Unite1TypePairInputs (..), Divorce1TypePairInputs (..),
	Unite2TypePairInputs (..), Divorce2TypePairInputs (..),
)
where

import Data.Kind (Type)

import qualified Control.Category as Base
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2

type TypePair = (Type, Type)

type family Fst (xy :: TypePair) where Fst '(x, y) = x
type family Snd (xy :: TypePair) where Snd '(x, y) = y


-- unite and divorce TypePair inputs

newtype Unite1TypePairInputs f t1 =
	Unite1TypePairInputs { deUnite1TypePairInputs :: f (Fst t1) (Snd t1) }

newtype Divorce1TypePairInputs f t11 t12 =
	Divorce1TypePairInputs { deDivorce1TypePairInputs :: f '(t11, t12) }

newtype Unite2TypePairInputs f t1 t2 =
	Unite2TypePairInputs { deUnite2TypePairInputs :: f (Fst t1) (Snd t1) (Fst t2) (Snd t2) }

newtype Divorce2TypePairInputs f t11 t12 t21 t22 =
	Divorce2TypePairInputs { deDivorce2TypePairInputs :: f '(t11, t12) '(t21, t22) }


-- instances

instance Cat2.Category c => Base.Category (Unite2TypePairInputs c) where
	id = Unite2TypePairInputs (Cat2.empty)
	(Unite2TypePairInputs x) . (Unite2TypePairInputs y) = Unite2TypePairInputs (Cat2.compose y x)
