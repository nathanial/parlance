/-
  Parlance.Parse.Extractor - Type-safe value extraction from parse results
-/

import Parlance.Core.Types

namespace Parlance

/-- Typeclass for types that can be parsed from command-line argument strings -/
class FromArg (α : Type) where
  /-- Parse a string to this type -/
  parse : String → Option α
  /-- The ArgType this corresponds to (for error messages) -/
  argType : ArgType

instance : FromArg String where
  parse s := some s
  argType := .string

instance : FromArg Int where
  parse s := s.toInt?
  argType := .int

instance : FromArg Nat where
  parse s := s.toNat?
  argType := .nat

instance : FromArg Float where
  parse s :=
    -- Handle decimal numbers like "0.7" or "1.5"
    match s.splitOn "." with
    | [intPart] => intPart.toNat?.map Float.ofNat
    | [intPart, fracPart] =>
      let intVal := intPart.toNat?.getD 0
      let fracVal := fracPart.toNat?.getD 0
      let fracDigits := fracPart.length
      if fracDigits == 0 then some (Float.ofNat intVal)
      else
        let divisor := Float.pow 10.0 (Float.ofNat fracDigits)
        some (Float.ofNat intVal + Float.ofNat fracVal / divisor)
    | _ => none
  argType := .float

instance : FromArg Bool where
  parse
    | "true" | "yes" | "1" | "on" => some true
    | "false" | "no" | "0" | "off" => some false
    | _ => none
  argType := .bool

instance : FromArg System.FilePath where
  parse s := some (System.FilePath.mk s)
  argType := .path

-- Extension methods for ParseResult with type-safe extraction
namespace ParseResult

/-- Get a typed value from the parse result -/
def get [FromArg α] (r : ParseResult) (name : String) : Option α :=
  r.values.getValue name >>= FromArg.parse

/-- Get a typed value with a default -/
def getD [FromArg α] (r : ParseResult) (name : String) (default : α) : α :=
  r.get name |>.getD default

/-- Get a typed value, panicking if not present or invalid -/
def get! [FromArg α] [Inhabited α] (r : ParseResult) (name : String) : α :=
  r.get name |>.getD default

/-- Get a boolean flag (present = true, absent = false) -/
def getBool (r : ParseResult) (name : String) : Bool :=
  if r.values.hasBool name then true
  else (r.values.getValue name >>= FromArg.parse (α := Bool)) |>.getD false

/-- Get a list of values (for flags that can be repeated) -/
def getList [FromArg α] (r : ParseResult) (name : String) : List α :=
  -- For now, just return single value as list
  -- A more complete implementation would track multiple values
  match r.get name with
  | some v => [v]
  | none => []

/-- Get a FilePath value -/
def getPath (r : ParseResult) (name : String) : Option System.FilePath :=
  r.get name

/-- Get a FilePath value with default -/
def getPathD (r : ParseResult) (name : String) (default : System.FilePath) : System.FilePath :=
  r.getPath name |>.getD default

/-- Get an Int value -/
def getInt (r : ParseResult) (name : String) : Option Int :=
  r.get name

/-- Get an Int value with default -/
def getIntD (r : ParseResult) (name : String) (default : Int) : Int :=
  r.getInt name |>.getD default

/-- Get a Nat value -/
def getNat (r : ParseResult) (name : String) : Option Nat :=
  r.get name

/-- Get a Nat value with default -/
def getNatD (r : ParseResult) (name : String) (default : Nat) : Nat :=
  r.getNat name |>.getD default

/-- Get a Float value -/
def getFloat (r : ParseResult) (name : String) : Option Float :=
  r.get name

/-- Get a Float value with default -/
def getFloatD (r : ParseResult) (name : String) (default : Float) : Float :=
  r.getFloat name |>.getD default

end ParseResult

end Parlance
