module Fana.Serial.Bidir.Parse
(
	ErrorText, Error (..), CanFail,
	Parser, parse,
	Result,
)
where

import Fana.Prelude
import Prelude (fst, String)

import qualified Control.Monad.State.Lazy as Monad
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show
import qualified Prelude as Base


type ErrorText = String

data Error position = Error
	{
	{- | The position in the input stream where the failed parser started. -}
	error_position :: Maybe position,
	error_content :: Accu.Accumulated ErrorText
	}
type CanFail p = Either [Error p]

instance Show.Showable ErrorText p => Show.Showable ErrorText (Error p) where
	show (Error pos content) =
		let at_pos_prefix p = "at " <> Fana.show p <> " : "
			in (Base.maybe "" at_pos_prefix pos) <> content

{-|
	Parser.

	* "p" is id of position in input stream.
	* "v" is the type of parsed value.

	The state monad covers the input stream.
	The Either monad handles parse errors.

	The state monad's structure is @(s -> m (v, s))@,
	where m is the either monad.
	Thus failed parsings do not alter the input stream.
-}
type Parser p v = Monad.StateT [p] (CanFail p) v

type Result p v = CanFail p v

parse :: Parser p v -> [p] -> Result p v
parse parser = Monad.runStateT parser >>> map fst
