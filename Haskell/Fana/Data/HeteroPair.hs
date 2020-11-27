module Fana.Data.HeteroPair
(
	swap,
	make,
	before, after,
)
where


import Prelude (flip)


swap :: (x, y) -> (y, x)
swap (x, y) = (y, x)

make :: x -> y -> (x, y)
make x y = (x, y)

before :: y -> x -> (x, y)
before = flip make

after :: x -> y -> (x, y)
after = make

