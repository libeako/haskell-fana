{-| Tools for testing serializers. -}
module Fana.Serial.Bidir.Test
(
	test_render_parse,
	test_parse_fail,
	test_serializer,
)
where

import Fana.Convert (ExistsConversion (..))
import Fana.Prelude
import Fana.Serial.Bidir.Serializer
import Fana.Serial.Bidir.Parse (parse)

import qualified Data.Foldable as Fold
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Fana.Serial.Bidir.Parse as Parse


-- | A render then parse loop.
render_parse :: Serializer c c v v' -> v -> Parse.Result c v'
render_parse s = renderer s >>> map toList >>> Fold.concat >>> parse (parser s)

-- | Test whether the parser can process what the renderer outputs.
test_render_parse :: (Eq v, ExistsConversion v' v) => Serializer c c v v' -> v -> Bool
test_render_parse s value = either (const False) (convert >>> (== value)) (render_parse s value)

-- | Expects the given parser to fail on the given input.
test_parse_fail :: Parse.Parser c v -> [c] -> Bool
test_parse_fail parser' = map Either.isLeft (Parse.parse parser')

{-|
	@test_serializer serializer highs lows@
	performs a render-then-parse loop test on all highs
	and a parse-fail test on all lows.
-}
test_serializer :: (Eq v, ExistsConversion v' v) => Serializer c c v v' -> [v] -> [[c]] -> Bool
test_serializer s highs lows =
	(List.all (test_render_parse s) highs) &&
	(List.all (test_parse_fail (parser s)) lows)
