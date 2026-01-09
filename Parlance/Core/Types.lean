/-
  Parlance.Core.Types - Core type definitions for CLI parsing
-/

namespace Parlance

/-- Types that can be parsed from command-line arguments -/
inductive ArgType where
  | string                          -- Raw string
  | int                             -- Integer
  | nat                             -- Natural number
  | float                           -- Floating point
  | bool                            -- Boolean (true/false/yes/no/1/0)
  | path                            -- File/directory path
  | choice (options : List String)  -- Enumerated values
  deriving Repr, BEq, Inhabited

namespace ArgType

def toString : ArgType → String
  | .string => "STRING"
  | .int => "INT"
  | .nat => "NAT"
  | .float => "FLOAT"
  | .bool => "BOOL"
  | .path => "PATH"
  | .choice opts => s!"({", ".intercalate opts})"

instance : ToString ArgType := ⟨ArgType.toString⟩

end ArgType

/-- A flag definition (short and/or long form) -/
structure Flag where
  /-- Long form name (e.g., "verbose" for --verbose) -/
  long : String
  /-- Optional short form (e.g., 'v' for -v) -/
  short : Option Char := none
  /-- Type of value this flag takes (none for boolean flags) -/
  argType : Option ArgType := none
  /-- Description for help text -/
  description : String := ""
  /-- Default value if not provided -/
  defaultValue : Option String := none
  /-- Whether this flag is required -/
  required : Bool := false
  /-- Environment variable to use as fallback (e.g., "TOKEN" for $TOKEN) -/
  envVar : Option String := none
  /-- Whether this flag can be specified multiple times -/
  repeatable : Bool := false
  /-- Whether this boolean flag supports --no-<flag> -/
  negatable : Bool := false
  /-- Custom validation function -/
  validate : Option (String → Except String Unit) := none
  deriving Inhabited

namespace Flag

/-- Check if a flag matches a given name (long or short) -/
def matchesName (f : Flag) (name : String) : Bool :=
  f.long == name || f.short.map (String.singleton) == some name

/-- Is this a boolean flag (no value required)? -/
def isBoolean (f : Flag) : Bool := f.argType.isNone

end Flag

/-- A positional argument definition -/
structure Arg where
  /-- Name of the argument (for help and extraction) -/
  name : String
  /-- Type of the argument -/
  argType : ArgType := .string
  /-- Description for help text -/
  description : String := ""
  /-- Whether this argument is required -/
  required : Bool := true
  /-- Default value if not provided -/
  defaultValue : Option String := none
  /-- Custom validation function -/
  validate : Option (String → Except String Unit) := none
  deriving Inhabited

/-- A complete command definition -/
structure Command where
  /-- Command name -/
  name : String
  /-- Version string -/
  version : String := ""
  /-- Description for help text -/
  description : String := ""
  /-- Available flags -/
  flags : Array Flag := #[]
  /-- Positional arguments -/
  args : Array Arg := #[]
  /-- Available subcommands -/
  subcommands : Array Command := #[]
  deriving Inhabited

namespace Command

/-- Find a flag by long name -/
def findFlagLong (cmd : Command) (name : String) : Option Flag :=
  cmd.flags.find? (·.long == name)

/-- Find a flag by short name -/
def findFlagShort (cmd : Command) (c : Char) : Option Flag :=
  cmd.flags.find? (·.short == some c)

/-- Find a subcommand by name -/
def findSubcommand (cmd : Command) (name : String) : Option Command :=
  cmd.subcommands.find? (·.name == name)

/-- Check if command has any subcommands -/
def hasSubcommands (cmd : Command) : Bool :=
  !cmd.subcommands.isEmpty

end Command

/-- Parsed values from command-line arguments -/
structure ParsedValues where
  /-- List of (name, value) pairs -/
  values : List (String × String) := []
  /-- Boolean flags that were present -/
  boolFlags : List String := []
  deriving Repr, Inhabited

namespace ParsedValues

def empty : ParsedValues := { values := [], boolFlags := [] }

def setValue (pv : ParsedValues) (name : String) (value : String) : ParsedValues :=
  -- Remove existing value for this name, then add new one
  let filtered := pv.values.filter (·.1 != name)
  { pv with values := filtered ++ [(name, value)] }

def setBool (pv : ParsedValues) (name : String) : ParsedValues :=
  let filtered := pv.values.filter (·.1 != name)
  if pv.boolFlags.contains name then
    { pv with values := filtered }
  else
    { pv with values := filtered, boolFlags := pv.boolFlags ++ [name] }

def clearBool (pv : ParsedValues) (name : String) : ParsedValues :=
  { pv with boolFlags := pv.boolFlags.filter (· != name) }

def getValue (pv : ParsedValues) (name : String) : Option String :=
  pv.values.find? (·.1 == name) |>.map (·.2)

def hasBool (pv : ParsedValues) (name : String) : Bool :=
  pv.boolFlags.contains name

/-- Add a value for a flag (appends without filtering, for repeatable flags) -/
def addValue (pv : ParsedValues) (name : String) (value : String) : ParsedValues :=
  { pv with values := pv.values ++ [(name, value)] }

/-- Get all values for a name (for repeatable flags) -/
def getValues (pv : ParsedValues) (name : String) : List String :=
  pv.values.filter (·.1 == name) |>.map (·.2)

/-- Check if any values exist for a name -/
def hasValue (pv : ParsedValues) (name : String) : Bool :=
  pv.values.any (·.1 == name)

end ParsedValues

/-- Result of successfully parsing a command -/
structure ParseResult where
  /-- Which command/subcommand was invoked (empty list = root command) -/
  commandPath : List String := []
  /-- All parsed flag and argument values -/
  values : ParsedValues := ParsedValues.empty
  /-- Remaining unparsed positional arguments -/
  remaining : List String := []
  deriving Repr, Inhabited

namespace ParseResult

/-- Get the invoked command name (last in path, or root command) -/
def commandName (pr : ParseResult) : Option String :=
  pr.commandPath.getLast?

/-- Check if a boolean flag was set -/
def hasFlag (pr : ParseResult) (name : String) : Bool :=
  pr.values.hasBool name

/-- Get a string value -/
def getString (pr : ParseResult) (name : String) : Option String :=
  pr.values.getValue name

/-- Get a string value with default -/
def getString! (pr : ParseResult) (name : String) (default : String := "") : String :=
  pr.getString name |>.getD default

end ParseResult

end Parlance
