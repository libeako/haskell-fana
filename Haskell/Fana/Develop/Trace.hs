module Fana.Develop.Trace
(
	trace_with,
)
where


import Prelude (Show)
import qualified Debug.Trace as Base


trace_with :: Show s => (t -> s) -> t -> t
trace_with f x = Base.traceShow (f x) x
