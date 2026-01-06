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

private def parseFloatStr (s : String) : Option Float :=
  if s.isEmpty then none
  else
    -- Handle optional sign
    let (isNeg, rest) := if s.startsWith "-" then (true, s.drop 1)
                         else if s.startsWith "+" then (false, s.drop 1)
                         else (false, s)
    -- Split on 'e' or 'E' for scientific notation
    let (mantissaStr, expStr) := match rest.splitOn "e" with
      | [m, e] => (m, some e)
      | [m] => match rest.splitOn "E" with
        | [m', e'] => (m', some e')
        | _ => (m, none)
      | _ => (rest, none)
    -- Parse the mantissa (integer and optional decimal parts)
    match mantissaStr.splitOn "." with
    | [intPart, fracPart] =>
      let intVal := if intPart.isEmpty then 0 else intPart.toNat?.getD 0
      let (fracVal, fracDigits) := if fracPart.isEmpty then (0, 0)
                                   else (fracPart.toNat?.getD 0, fracPart.length)
      let baseVal := Float.ofNat intVal + Float.ofNat fracVal / Float.pow 10.0 (Float.ofNat fracDigits)
      let signedVal := if isNeg then -baseVal else baseVal
      applyExponent signedVal expStr
    | [intPart] =>
      match intPart.toNat? with
      | some n =>
        let signedVal := if isNeg then -(Float.ofNat n) else Float.ofNat n
        applyExponent signedVal expStr
      | none => none
    | _ => none
where
  applyExponent (val : Float) (expStr : Option String) : Option Float :=
    match expStr with
    | some e =>
      let (expNeg, expRest) := if e.startsWith "-" then (true, e.drop 1)
                               else if e.startsWith "+" then (false, e.drop 1)
                               else (false, e)
      match expRest.toNat? with
      | some exp => some (if expNeg then val / Float.pow 10.0 (Float.ofNat exp)
                          else val * Float.pow 10.0 (Float.ofNat exp))
      | none => some val
    | none => some val

instance : FromArg Float where
  parse := parseFloatStr
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

/-- Get all raw string values for a repeatable flag -/
def getValues (r : ParseResult) (name : String) : List String :=
  r.values.getValues name

/-- Get all typed values for a repeatable flag -/
def getAll [FromArg α] (r : ParseResult) (name : String) : List α :=
  r.values.getValues name |>.filterMap FromArg.parse

/-- Get a list of values (for flags that can be repeated) -/
def getList [FromArg α] (r : ParseResult) (name : String) : List α :=
  r.getAll name

/-- Get all string values for a repeatable flag -/
def getStrings (r : ParseResult) (name : String) : List String :=
  r.getValues name

/-- Get all integer values for a repeatable flag -/
def getInts (r : ParseResult) (name : String) : List Int :=
  r.getAll name

/-- Get all natural number values for a repeatable flag -/
def getNats (r : ParseResult) (name : String) : List Nat :=
  r.getAll name

/-- Get all float values for a repeatable flag -/
def getFloats (r : ParseResult) (name : String) : List Float :=
  r.getAll name

/-- Get all path values for a repeatable flag -/
def getPaths (r : ParseResult) (name : String) : List System.FilePath :=
  r.getAll name

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
