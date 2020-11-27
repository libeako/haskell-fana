{-|
	For each optic category Cat a main data type exists.

	For each category named Cat an interface CatI exists that exposes the functionality of Cat.

	Cat and the types in the subcategories implement CatI.
	Hence for example :
	though traverse is a method of IsTraversal : one can invoke fill on a Lens too,
	because Lens is an instance of IsTraversal.

	Mathematically : optics form categories.
	But technically my optic data types do not have an instance of the usual Category class.
	This is because the usual Category class assumes arrow types with 2 inputs [the source and target of arrows],
	but my optic data types have 2 * 2 = 4 inputs :

	* 2 for the 2 pieces of data that the optic relates together;
	* 2 for the 2 versions of the data [old and new].

	But to respect the important mathematical property of forming categories :
	my optic data types do have an instance of a Category typeclass,
	but of a technically different one, one that also assumes types with 2 * 2 inputs.
	This category class is defined in "Fana.Math.Algebra.Category.OnTypePairs" as such :

	>	class Category c where
	>		empty :: c t1 t2 t1 t2
	>		compose :: c x1 x2 y1 y2 -> c y1 y2 z1 z2 -> c x1 x2 z1 z2

	In addition convenient infix symbols are defined :

	>	(>**>) = compose
	>	(<**<) = flip (>**>)


	This is the technical tool with which to compose optics from the same category.

	But in practice the situation is also abundant when the optics to be composed are from different categories.
	Any optic library must provide some facility far that.

	The solution here is a modified version of the categorical composition function,
	defined in "Fana.Math.Algebra.Category.ConvertThenCompose" :

	This solution works robustly in practice.
	The trade-off of having to write an initial empty optic in the chain is small,
	in my opinion is worth not having to use profunctor representation.
-}
module Fana.Optic.Concrete.AaaReadme where
