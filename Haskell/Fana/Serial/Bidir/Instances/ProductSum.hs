module Fana.Serial.Bidir.Instances.ProductSum
(
	product, sum,
)
where

import Control.Arrow ((***))
import Data.Semigroup hiding (Sum, Product)
import Data.Traversable ()
import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Parse (Parser, Error (..))

import qualified Control.Monad.Except as Monad


type Product f x y = Combiner f x y (x, y)
type Sum f x y = Combiner f x y (Either x y)


render_product :: Product (Renderer c) x y
render_product = uncurry (***) >>> map (uncurry (<>))

parse_product :: Product (Parser p) x y
parse_product = uncurry (liftA2 (,))

-- | The product [sequence] of 2 other serializers.
product ::
	forall c c' x x' y y' .
	(Serializer c c' x x', Serializer c c' y y') -> Serializer c c' (x, y) (x', y')
product = combine render_product parse_product

{-|
	This combinator does not propagate its child errors
	because it can not choose from them.
-}
parse_sum :: Sum (Parser p) x y
parse_sum (px, py) =
	let
		epx = map Left px
		epy = map Right py
		err :: Error p
		err = Error Nothing ""
		in
			-- any error report to outside should be neutral;
			Monad.catchError (Monad.catchError epx (const epy)) (const (Monad.throwError [err]))

-- | The sum [choice] of 2 other serializers.
sum ::
	forall c c' x x' y y' .
	(Serializer c c' x x', Serializer c c' y y') -> Serializer c c' (Either x y) (Either x' y')
sum = combine (uncurry either) parse_sum
