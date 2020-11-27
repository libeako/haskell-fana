module Fana.OperatingSystem.ExitCode
(
	from_ExitCode_to_Either,
	from_Either_to_ExitCode,
	iso_with_Either,
)
where

import System.Exit
import Fana.Prelude.FromBase
import Fana.Optic.Concrete.Prelude

import qualified Data.Int as Int


type FailCode = Int.Int

from_ExitCode_to_Either :: ExitCode -> Either FailCode ()
from_ExitCode_to_Either =
	\ case
		ExitSuccess -> Right ()
		ExitFailure code -> Left code

from_Either_to_ExitCode :: Either FailCode e -> ExitCode
from_Either_to_ExitCode = either ExitFailure (const ExitSuccess)

iso_with_Either :: Iso' ExitCode (Either FailCode ())
iso_with_Either = Iso from_Either_to_ExitCode from_ExitCode_to_Either
