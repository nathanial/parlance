/-
  Parlance.Core.Error - Parse error types
-/

import Parlance.Core.Types

namespace Parlance

/-- Errors that can occur during argument parsing -/
inductive ParseError where
  /-- Unknown flag was provided -/
  | unknownFlag (flag : String)
  /-- Flag requires a value but none was provided -/
  | missingValue (flag : String)
  /-- Value could not be parsed to the expected type -/
  | invalidValue (flag : String) (value : String) (expected : ArgType)
  /-- Required argument was not provided -/
  | missingRequired (name : String)
  /-- Unknown subcommand -/
  | unknownCommand (cmd : String) (available : List String)
  /-- Too many positional arguments -/
  | tooManyPositional (max : Nat) (got : Nat)
  /-- Value not in allowed choices -/
  | invalidChoice (flag : String) (value : String) (options : List String)
  /-- Custom validation failed -/
  | validationFailed (name : String) (value : String) (message : String)
  /-- Help was requested -/
  | helpRequested
  /-- Version was requested -/
  | versionRequested
  deriving Repr, BEq, Inhabited

namespace ParseError

def toString : ParseError → String
  | .unknownFlag f => s!"Unknown flag: --{f}"
  | .missingValue f => s!"Flag '--{f}' requires a value"
  | .invalidValue f v t => s!"Invalid value '{v}' for '--{f}': expected {t}"
  | .missingRequired n => s!"Missing required argument: {n}"
  | .unknownCommand c avail =>
    let availStr := if avail.isEmpty then "" else s!"\nAvailable commands: {", ".intercalate avail}"
    s!"Unknown command: {c}{availStr}"
  | .tooManyPositional m g => s!"Too many positional arguments: expected at most {m}, got {g}"
  | .invalidChoice f v opts => s!"Invalid value '{v}' for '--{f}': must be one of [{", ".intercalate opts}]"
  | .validationFailed n v msg => s!"Validation failed for '{n}' with value '{v}': {msg}"
  | .helpRequested => "Help requested"
  | .versionRequested => "Version requested"

instance : ToString ParseError := ⟨ParseError.toString⟩

/-- Check if this error is a special request (help/version) rather than an actual error -/
def isRequest : ParseError → Bool
  | .helpRequested => true
  | .versionRequested => true
  | _ => false

end ParseError

end Parlance
