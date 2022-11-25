module Fana.PropertyTree.Serialize
(
	ParseError, 
	Serializer, 
	atomic_serializer,
	FieldsParseResult, FieldsParser, Field (..), RecordType, 
	record_serializer_over_property_list, record_serializer,
)
where

import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Monoid (appEndo)
import Fana.Prelude
import Fana.PropertyTree.Data
import Prelude (Char, String)

import qualified Data.Monoid as Monoid
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Key.LensToMaybeElement as Map
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type ParseError = String

type SerializerBetween l h = Optic.PartialIso' ParseError l h
type Serializer t = SerializerBetween PropertyValue t


{-| Serializer of an atomic property value. -}
atomic_serializer :: forall t . SerializerBetween String t -> Serializer t
atomic_serializer s =
	let
		r :: t -> PropertyValue
		r = Optic.down s >>> PropertyValueAtomic
		p :: PropertyValue -> Either ParseError t
		p =
			\case
				PropertyValueAtomic v -> Optic.interpret s v
				PropertyValueComposite _ ->
					Base.error "BUG. An atomic property value was supposed but encountered a composite one."
		in Optic.PartialIso r p

type FieldsParseResult t = Either ParseError (t -> t)
type FieldsParser t = PropertyValue -> FieldsParseResult t

data Field r =
	forall f .
	Field
	{ field_name :: String
	, field_lens :: Optic.Lens' f r
	, field_serializer :: Serializer f
	}

type RecordType r = StringyMap.Map Char (Field r)

find_field :: RecordType r -> PropertyName -> Field r
find_field m n =
	fromMaybe
		(Base.error ("field name " <> n <> " is not recognized"))
		(Map.get_at n m)

render_field :: r -> Field r -> Property
render_field r (Field n l s) = (n, Optic.down s (Optic.down l r))

parse_field_value :: forall r . Field r -> PropertyValue -> Either ParseError (Fn.Endo r)
parse_field_value (Field n l s) = Optic.interpret s >>> map (Optic.fill l)

parse_field :: RecordType r -> Property -> Either ParseError (Fn.Endo r)
parse_field m (n, v) = parse_field_value (find_field m n) v

record_serializer_over_property_list :: 
	forall r . Default r => RecordType r -> SerializerBetween [Property] r
record_serializer_over_property_list rt =
	let
		r :: r -> [Property]
		r d = map (render_field d) (toList rt)
		p :: [Property] -> Either ParseError r
		p list =
			let
				field_values :: Either ParseError [Fn.Endo r]
				field_values = traverse (parse_field rt) list
				in map (foldMap Monoid.Endo >>> appEndo >>> ($ def)) field_values
		in Optic.PartialIso r p

record_serializer :: forall r . Default r => RecordType r -> Serializer r
record_serializer rt =
	let
		opl = record_serializer_over_property_list rt
		r :: r -> PropertyValue
		r d = PropertyValueComposite (Optic.down opl d)
		p :: PropertyValue -> Either ParseError r
		p =
			\case
				PropertyValueAtomic _ -> 
					Base.error "BUG. A composite property value was supposed but encountered an atomic one."
				PropertyValueComposite list -> Optic.interpret opl list
		in Optic.PartialIso r p
