{-| The most basic serializers. -}
module Fana.Serial.Bidir.Instances.Basic
(
	nothing, atom, whole,
)
where

import Control.Applicative (pure)
import Data.Traversable ()
import Fana.Convert (ExistsConversion (..))
import Fana.Serial.Bidir.Parse (Parser)
import Fana.Serial.Bidir.Serializer
import Fana.Prelude

import qualified Control.Monad.Except as Monad
import qualified Control.Monad.State as Monad
import qualified Data.List.NonEmpty as Base
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Tree.Leaf as Tree
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Parse as Parse
import qualified Fana.Serial.Print.Show as Show
import qualified Data.String as Base


render_nothing :: Renderer c ()
render_nothing = const []

-- | Nothing. Does not do anything.
nothing :: Serializer c c' () ()
nothing = Serializer render_nothing (pure ())


render_atom :: Renderer c c
render_atom = Tree.leaf >>> (: [])

parse_atom :: forall c c' . ExistsConversion c' c => Parse.Parser c' c
parse_atom =
	let
		parser_raw :: [c'] -> Either [Parse.Error c'] (c, [c'])
		parser_raw = \ case
			[] -> Left [Parse.Error Nothing "input stream end reached, no more input"]
			c : rest -> Right (convert c, rest)
		in Monad.StateT parser_raw

-- | Atom, a single element of the stream.
atom :: ExistsConversion c' c => Serializer c c' c c
atom = Serializer render_atom parse_atom


-- erroring on some input stream remaining after parsing

data InputStreamNotConsumed c = InputStreamNotConsumed { remained_stream :: Base.NonEmpty c }

instance (Base.IsString b, Show.Showable b c) => Show.Showable b (InputStreamNotConsumed c) where
	show (InputStreamNotConsumed rest) =
		"some input stream remains [" <> foldMap Show.show rest <> "]"

parse_not_consumed_input_stream_as_error ::
	Show.Showable Base.String c => Base.NonEmpty c -> Parser c v
parse_not_consumed_input_stream_as_error non_empty_input =
	let
		error =
			Parse.Error
				(Just (Base.head non_empty_input))
				(Show.show (InputStreamNotConsumed non_empty_input))
		in Monad.throwError [error]

parse_whole :: Show.Showable Base.String c => Fn.Endo (Parser c v)
parse_whole old_parse =
	do
		parsed_value <- old_parse
		input_stream <- Monad.get
		case Base.nonEmpty input_stream of
			Nothing -> pure parsed_value
			Just non_empty_input ->
				parse_not_consumed_input_stream_as_error non_empty_input

{-| The output serializer is like the input one but errors if some of the input stream remains. -}
whole :: forall c c' v v' . Show.Showable Base.String c' => Fn.Endo (Serializer c c' v v')
whole = Optic.fn_up parser_in_Serializer parse_whole
