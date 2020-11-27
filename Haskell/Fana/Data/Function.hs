module Fana.Data.Function
(
	Endo, Combiner,
	Hop, FnUpTo,
	UpTo, DownFrom,
	UpToForall (..), DownFromForall (..),

	(*>>>), (<<<*),

	contramap_2,
)
where


-- Useful function types

type Endo x = x -> x
type Combiner x = x -> x -> x
type UpTo f x = x -> f x
type DownFrom f x = f x -> x
type Hop c1 c2 x = c1 x -> c2 x
{-| Lifts a function type up to f. -}
type FnUpTo f x y = (x -> y) -> (f x -> f y)

newtype UpToForall c = UpToForall { unwrapUpToForall :: forall x . UpTo c x }
newtype DownFromForall c = DownFromForall { unwrapDownFromForAll :: forall x . DownFrom c x }


-- Function application and composition operators

infixl 0 *>>>
infixr 0 <<<*

(*>>>) :: x -> (x -> y) -> y
(<<<*) :: (x -> y) -> x -> y

x *>>> f = f x
f <<<* x = f x


{-| Same as @flip on@ -}
contramap_2 :: (x2 -> x1) -> (x1 -> x1 -> y) -> (x2 -> x2 -> y)
contramap_2 t f = \ x1 x2 -> f (t x1) (t x2)


