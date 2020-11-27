{-|
	= The Representation

	In my opinion both the concrete and the profunctor representation of optics have advantages over the other.
	But in practice mostly the concrete representation is needed, is better in my opinion.

	This library contains a profunctor implementation too, but it is not finished, is not polished,
	may change any time without even a version bumb. Do NOT use it.
	This is also true for the Laarhoven representation. Do NOT use it.

	I am currently happy about the state of the concrete representation.

	= Naming

	Optics link 2 pieces of data which are often in a container-element relationship with each other.
	Because of this : the names of these data roles in this library are often by their level :
	("l" = "low" [contained element]) and ("h" = "high" [container]).
-}
module Fana.Optic.AaaReadme where
