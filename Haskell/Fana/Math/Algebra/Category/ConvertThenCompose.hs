{-|
	Conversion among things that form categories,
	categories which are in a hierarchy.

	This module provides some composition functions. As infix symbols.
	These differ from the well-known categorical composition function [@>>>@]
	mainly by allowing the composition of arrows with different categories,
	provided that those categories are still connected via some convertability.
	They convert their second input to the category of their first input.
	Thus in a chain of compositions the category gets propagated
	from the first arrow in the chain to the rest, one by one.
	If the category of your first arrow happens to not have the intended category of your whole chain
	then put a category-polymorphic identity arrow into the chain as the very first arrow
	- that will pick up whatever category from the context.

	Here is an example :

	@
		identity_arrow
		>**>^ my_lens
		>**>^ my_prism
	@

	If the context expects a traversal then
	@identity_arrow@ will be instantiated as an arrow in the category of traversals.
	Both lenses and prisms are convertable to traversals and those conversions are performed.
	Thus every arrow in this composition chain gets converted to traversal
	and the traversals get composed.

	Flipped versions of these conversion functions are not needed
	and i felt that it is a good practice
	to stick to one input order for code uniformity.
-}
module Fana.Math.Algebra.Category.ConvertThenCompose
(
	(>>>^), (>**>^),
)
where


import Control.Category (Category (..))
import Fana.Haskell.DescribingClass
import Fana.Prelude.FromBase

import qualified Fana.Math.Algebra.Category.OnTypePairs as Tp


infixl 5 >>>^
infixl 5 >**>^


{-|
	Convert then compose arrows between types.
-}
(>>>^) ::
	(Category a, HasDescribingClass2 a ) =>
	DescribingClass2Of a b =>
	a p1 p2 -> b p2 p3 -> a p1 p3
(>>>^) a b = a >>> convert_from_describing_class_2 b

{-|
	Convert then compose arrows between type pairs.
-}
(>**>^) ::
	(Tp.Category a, HasDescribingClass4 a) =>
	DescribingClass4Of a b =>
	a p11 p12 p21 p22 -> b p21 p22 p31 p32 -> a p11 p12 p31 p32
(>**>^) a b = Tp.compose a (convert_from_describing_class_4 b)
