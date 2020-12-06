module Fana.Optic.Profunctor.Access.Over
(
	over, traverse, fold, fold_map,
)
where

import Fana.Prelude hiding (traverse, fold)
import Fana.Optic.Profunctor.Optic
import Fana.Optic.Profunctor.Categories
import Data.Functor.Const (Const (..))

import qualified Fana.Optic.Concrete.Prelude as Concrete
import qualified Fana.Data.FunctionUpIo as FunUpIo


over :: OpticBone (->) p1 p2 w1 w2 -> (p1 -> p2) -> (w1 -> w2)
over o = o

traverse :: Applicative f => Traversal p1 p2 w1 w2 -> (p1 -> f p2) -> (w1 -> f w2)
traverse = Concrete.fn_down FunUpIo.iso_UpO

fold :: Monoid m => Traversal m m' w w' -> (w -> m)
fold o = (traverse o Const) >>> getConst

fold_map :: Monoid m => Traversal p m w w' -> (p -> m) -> (w -> m)
fold_map o f = (traverse o (f >>> Const)) >>> getConst

