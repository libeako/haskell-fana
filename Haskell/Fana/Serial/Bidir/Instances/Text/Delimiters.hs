module Fana.Serial.Bidir.Instances.Text.Delimiters
(
    words, lines,
)
where

import Fana.Prelude
import Prelude (Char, String)

import qualified Data.Foldable as Base
import qualified Data.List as List
import qualified Fana.Optic.Concrete.Categories.Iso as Optic


words :: Optic.Iso' String [String]
words = Optic.Iso (List.intercalate " ") List.words

line_end_char :: Char
line_end_char = '\n'

lines :: Optic.Iso' String [String]
lines =
    let
	concat :: [String] -> String
	concat = map (: [[line_end_char]]) >>> Base.concat >>> Base.concat
	in Optic.Iso concat List.lines

