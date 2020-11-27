{-|
	A very simple testing library.
	Its only feature is that tests can be bunched together hierarchically.
	Great (usefulness / complexity) value.
	Perfect for novice haskell users.
-}
module Fana.Develop.Test.Define
(
	Text, Name, PassValue,
	Test,
	-- * Constructors
	single, bunch,
)
where


import Fana.Prelude

import qualified Data.String as Base
import qualified Fana.Data.Tree.Discriminating as DTree


type Text = Base.String
type Name = Text
{-|
	Whether the test is passed.

	* True = Success
	* False = Failure

	Boolean blindness here is not big problem,
	because of the convention that test values are defined by assertions.
-}
type PassValue = Bool

type Test = DTree.Tree [] PassValue () Name

-- * Constructors

-- | Creates a test consisting of a single simple test.
single :: Name -> PassValue -> Test
single name value = DTree.assemble (DTree.Leaf value) name

-- | Bunches some tests into a composite one.
bunch :: Name -> [Test] -> Test
bunch name children = DTree.assemble (DTree.Joint () children) name
