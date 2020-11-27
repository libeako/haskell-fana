module Fana.Serial.Print.Show
(
	from_Char,
	iso_String,
	from_Show,
	from_ShowS, to_ShowS, iso_ShowS,
	Showable (..),
)
where

import Control.Category (id, (>>>))
import Fana.Math.Algebra.Monoid.Accumulate
import Fana.Prelude.FromBase
import Prelude (($), ShowS, String)

import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Prelude as Base


from_Char :: Base.Char -> Accumulated String
from_Char = (: []) >>> single

iso_String :: Optic.Iso' String (Accumulated String)
iso_String = Optic.Iso extract single

from_Show :: Base.Show x => x -> Accumulated String
from_Show = Base.show >>> single

from_ShowS :: ShowS -> Accumulated String
from_ShowS = ($ "") >>> single

to_ShowS :: Accumulated String -> ShowS
to_ShowS = extract >>> (<>)

iso_ShowS :: Optic.Iso' ShowS (Accumulated String)
iso_ShowS = Optic.Iso to_ShowS from_ShowS


class Showable b t where
	show :: t -> Accumulated b

instance Showable b (Accumulated b) where show = id
instance Showable String Base.Char where show = from_Char
instance Showable String String where show = single
instance Showable String ShowS where show = from_ShowS
