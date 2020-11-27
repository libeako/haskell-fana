module Fana.Serial.Bidir.Instances.Decorate
(
	decorate,
)
where

import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Parse (Parser)
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Instances.WithStartPosition (with_start_position)

import qualified Control.Monad.Except as Monad
import qualified Fana.Data.Function as Fn
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Parse as Parse


type ErrorText = Parse.ErrorText
type DecoratedSerializationParseError = Accu.Accumulated ErrorText

{-|
	Decorates the input parser with description for the human.
	The description is useful in error report.
-}
parse_decorated :: Accu.Accumulated ErrorText -> Fn.Endo (Parser p v)
parse_decorated description old_parser =
	let
		parse' pos =
			let
				error_changer = (Parse.Error pos description :)
				in Monad.catchError old_parser (error_changer >>> Monad.throwError)
		in with_start_position parse'

{-|
	Decorates the input parser with description for the human.
	The description is useful in error report.

	Do not confuse this with @"tagged"@.
	That puts the given tag into the language.
	This one puts the given decoration only into error report.
-}
decorate :: Accu.Accumulated ErrorText -> Fn.Endo (Serializer c c' v v')
decorate = parse_decorated >>> Optic.fn_up parser_in_Serializer


