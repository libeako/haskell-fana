module Fana.Serial.Bidir.Instances.Maybe
(
	render_maybe, parse_if_can,
	trier,
)
where

import Control.Applicative hiding (many)
import Fana.Prelude.FromBase hiding (maybe)
import Fana.Serial.Bidir.Parse (Parser)
import Fana.Serial.Bidir.Serializer

import qualified Control.Monad.Except as Monad
import qualified Data.Maybe as Base


{-| Does not alter the stream if the value is @Nothing@. -}
render_maybe :: Renderer c v -> Renderer c (Maybe v)
render_maybe = Base.maybe []

{- |
	Tries to parse, swallows any error by returning @Nothing@.

	Note : this is not a parser of @Maybe@.
-}
parse_if_can :: Parser c v -> Parser c (Maybe v)
parse_if_can = map Just >>> flip Monad.catchError (const (pure Nothing))

{- |
	Tries to serialize.

	The renderer does not alter the stream if the value is @Nothing@.
	The parser tries to parse, swallows any error by returning @Nothing@.

	Note : this is not a serializer of @Maybe@.
-}
trier ::
	Serializer c c' v v' ->
	Serializer c c' (Maybe v) (Maybe v')
trier = transform render_maybe parse_if_can
