{-|
	This is the central module of the bidirectional serialization stuff,
	because it captures the essence of it with a data type :
	the aggregation of a renderer and a parser.
-}
module Fana.Serial.Bidir.Serializer
(
	Renderer,
	Serializer (..),
	Serializer',
	SerializerCompact,
	-- * Optics
	renderer_in_Serializer, parser_in_Serializer,
	-- * Transformation and Combination
	Transformer, transform,
	Combiner, combine,
	-- * Extension with [Partial] Isomorphism
	extend_with_iso,
	extend_with_partial_iso,
	-- * Relation with Partial Isomorphism
	to_partial_iso, from_partial_iso, iso_with_partial_iso,
)
where

import Control.Applicative
import Control.Arrow ((***))
import Fana.Haskell.TypePair (Fst, Snd)
import Fana.Math.Algebra.Category.Functor.Pro (Profunctor (..))
import Fana.Prelude
import Prelude (fst)

import qualified Control.Monad.Except as Mtl
import qualified Control.Monad.State.Lazy as Mtl
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.List as List
import qualified Fana.Data.Tree.Leaf as Leafy
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.WithStartPosition as Wsp
import qualified Fana.Serial.Bidir.Parse as Parse
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Base


{-|
	* "c" is the type of character in the stream.
-}
type SyntaxForest c = [Leafy.Tree [] c]

{-|
	Renderer.

	* "c" is the type of character in the stream.
	* "v" is the type of rendered value.
-}
type Renderer c v = v -> SyntaxForest c

{-|
	Serializer.
	Also known as : "bidirectional serializer" or "bidirectional parsing" or "pickering".

	* "c" is the type of character in the stream.
	* "c'" is the same, but possibly extended with parsing-specific data.
	* "v" is the type of value to be serialized.
	* "v'" is the same, but possibly extended with parsing-specific data.
-}
data Serializer c c' v v' =
	Serializer
	{
		renderer :: Renderer c v,
		parser :: Parse.Parser c' v'
	}

{-|
	The same serializer data type,
	just with a more compact type parameterization.
-}
type SerializerCompact c v = Serializer (Fst c) (Snd c) (Fst v) (Snd v)

{-| The serializer type that uses the same stream element types for rendering and parsing. -}
type Serializer' c v = Serializer c c v v


-- Optics

renderer_in_Serializer ::
	Optic.Lens
		(Renderer c1 v1) (Renderer c2 v2)
		(Serializer c1 c v1 v) (Serializer c2 c v2 v)
renderer_in_Serializer = Optic.lens_from_get_set renderer (\ r s -> s { renderer = r })

parser_in_Serializer ::
	Optic.Lens
		(Parse.Parser c1 v1) (Parse.Parser c2 v2)
		(Serializer c c1 v v1) (Serializer c c2 v v2)
parser_in_Serializer = Optic.lens_from_get_set parser (\ p r -> r { parser = p })


instance Profunctor (Serializer c c') where
	dimap fr fp (Serializer rr rp) =
		Serializer (fr >>> rr) (Mtl.StateT (Mtl.runStateT rp >>> map (Bifunctor.first fp)))


-- Transformation and Combination

type Transformer f x y = f x -> f y
type Combiner f x y r = (f x, f y) -> f r

transform ::
	(Transformer (Renderer c) x y) ->
	(Transformer (Parse.Parser c') x' y') ->
	(Transformer (SerializerCompact '(c, c')) '(x, x') '(y, y'))
transform tr tp r = Serializer (tr (renderer r)) (tp (parser r))

combine ::
	(Combiner (Renderer c) x y r) ->
	(Combiner (Parse.Parser c') x' y' r') ->
	(Combiner (SerializerCompact '(c, c')) '(x, x') '(y, y') '(r, r'))
combine cr cp =
	let
		rr = (renderer *** renderer) >>> cr
		pr = (parser *** parser) >>> cp
		in liftA2 Serializer rr pr


-- Relation with Partial Isomorphism

from_partial_iso ::
	Optic.PartialIso [Parse.ErrorText] [c] [c'] v v' -> Serializer c c' v v'
from_partial_iso (Optic.PartialIso r p) =
	let
		new_p is =
			let
				error = Accu.single >>> Parse.Error (List.first is)
				in bimap (map error) (Pair.before is) (p is)
		new_r = r >>> map Leafy.leaf >>> Leafy.joint >>> (: [])
		in Serializer new_r (Mtl.StateT new_p)

to_partial_iso ::
	forall c c' v v' .
	Serializer c c' v v' -> Optic.PartialIso [Parse.Error c'] [c] [c'] v v'
to_partial_iso (Serializer r p) =
	let
		new_r :: v -> [c]
		new_r = r >>> map Base.toList >>> Base.concat
		new_p :: [c'] -> Either [Parse.Error c'] v'
		new_p = Mtl.runStateT p >>> map fst
		in Optic.PartialIso new_r new_p

iso_with_partial_iso ::
	Optic.Iso
		(Optic.PartialIso [Parse.ErrorText] [c1] [c1'] v1 v1')
		(Optic.PartialIso [Parse.ErrorText] [c2] [c2'] v2 v2')
		(Serializer c1 c1' v1 v1')
		(Serializer c2 c2' v2 v2')
iso_with_partial_iso =
	let
		convert_error = Optic.piso_convert_error (map (Parse.error_content >>> fold))
		in Optic.Iso (to_partial_iso >>> convert_error) from_partial_iso



{-| Extends a serialization with an isomorphism. -}
extend_with_iso ::
	forall c c' x x' y y' .
	Optic.Iso x x' y y' ->
	Serializer c c' x x' ->
	Serializer c c' y y'
extend_with_iso iso = transform (Optic.down iso >>>) (map (Optic.up iso))

{-|	Extends a serialization with a partial isomorphism. -}
extend_with_partial_iso ::
	forall c c' l l' h h' .
	Optic.PartialIso Parse.ErrorText l l' h h' ->
	Serializer c c' l l' ->
	Serializer c c' h h'
extend_with_partial_iso extension (Serializer r p) =
	let
		parse :: Maybe c' -> Parse.Parser c' h'
		parse pos =
			let
				on_error :: Parse.ErrorText -> Parse.Parser c' h'
				on_error = Accu.single >>> Parse.Error pos >>> (: []) >>> Mtl.throwError
				in p >>= (Optic.piso_interpret extension >>> either on_error pure)
		in Serializer (Optic.piso_down extension >>> r) (Wsp.with_start_position parse)
