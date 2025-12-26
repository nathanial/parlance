/-
  Parlance.Parse.Parser - Core parsing logic
-/

import Parlance.Core.Types
import Parlance.Core.Error
import Parlance.Parse.Tokenizer

namespace Parlance.Parse

open Parlance

/-- Parser state during argument parsing -/
structure ParserState where
  /-- Remaining tokens to process -/
  tokens : List Token
  /-- Accumulated parsed values -/
  values : ParsedValues := {}
  /-- Command path (for subcommands) -/
  commandPath : List String := []
  /-- Current command being parsed -/
  currentCommand : Command
  /-- Positional arguments consumed -/
  positionalIndex : Nat := 0
  /-- Remaining positional arguments -/
  remaining : List String := []
  deriving Inhabited

/-- Parser monad -/
abbrev ParserM := ExceptT ParseError (StateM ParserState)

namespace ParserM

/-- Get the next token without consuming it -/
def peek : ParserM (Option Token) := do
  let state ← get
  pure state.tokens.head?

/-- Consume the next token -/
def advance : ParserM (Option Token) := do
  let state ← get
  match state.tokens with
  | [] => pure none
  | t :: rest =>
    set { state with tokens := rest }
    pure (some t)

/-- Record a value for a flag/argument -/
def setValue (name : String) (value : String) : ParserM Unit := do
  let state ← get
  set { state with values := state.values.setValue name value }

/-- Record a boolean flag -/
def setBool (name : String) : ParserM Unit := do
  let state ← get
  set { state with values := state.values.setBool name }

/-- Add to remaining positional args -/
def addRemaining (value : String) : ParserM Unit := do
  let state ← get
  set { state with remaining := state.remaining ++ [value] }

/-- Try to consume a flag value from the next token -/
def consumeFlagValue (flagName : String) : ParserM String := do
  let next ← advance
  match next with
  | some (.positional value) => pure value
  | some t =>
    -- Put it back - this is a bit awkward, we'll handle differently
    let state ← get
    set { state with tokens := t :: state.tokens }
    throw (.missingValue flagName)
  | none => throw (.missingValue flagName)

/-- Parse a long flag -/
def parseLongFlag (name : String) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Check for built-in flags
  if name == "help" || name == "h" then
    throw .helpRequested
  if name == "version" || name == "V" then
    throw .versionRequested

  match cmd.findFlagLong name with
  | some flag =>
    if flag.isBoolean then
      setBool flag.long
    else
      let value ← consumeFlagValue flag.long
      setValue flag.long value
  | none => throw (.unknownFlag name)

/-- Parse a long flag with attached value -/
def parseLongFlagValue (name : String) (value : String) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  match cmd.findFlagLong name with
  | some flag =>
    if flag.isBoolean then
      -- Boolean flag shouldn't have a value, but we'll accept it
      if value == "true" || value == "1" || value == "yes" then
        setBool flag.long
      -- Otherwise, don't set (treat as false)
    else
      setValue flag.long value
  | none => throw (.unknownFlag name)

/-- Parse a short flag -/
def parseShortFlag (c : Char) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Check for built-in flags
  if c == 'h' then
    throw .helpRequested
  if c == 'V' then
    throw .versionRequested

  match cmd.findFlagShort c with
  | some flag =>
    if flag.isBoolean then
      setBool flag.long
    else
      let value ← consumeFlagValue flag.long
      setValue flag.long value
  | none => throw (.unknownFlag (String.singleton c))

/-- Parse a positional argument -/
def parsePositional (value : String) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Check if this is a subcommand
  if state.positionalIndex == 0 && cmd.hasSubcommands then
    match cmd.findSubcommand value with
    | some subcmd =>
      set { state with
        currentCommand := subcmd
        commandPath := state.commandPath ++ [value]
      }
      return
    | none =>
      if !cmd.args.isEmpty then
        -- Not a subcommand, treat as positional
        pure ()
      else
        throw (.unknownCommand value (cmd.subcommands.toList.map (·.name)))

  -- Handle as positional argument
  if h : state.positionalIndex < cmd.args.size then
    let arg := cmd.args[state.positionalIndex]
    setValue arg.name value
    -- Get fresh state after setValue, otherwise we overwrite the value
    modify fun s => { s with positionalIndex := s.positionalIndex + 1 }
  else
    addRemaining value

/-- Parse a single token -/
def parseToken (token : Token) : ParserM Unit :=
  match token with
  | .shortFlag c => parseShortFlag c
  | .longFlag name => parseLongFlag name
  | .longFlagValue name value => parseLongFlagValue name value
  | .shortFlagValue c value => do
    let state ← get
    match state.currentCommand.findFlagShort c with
    | some flag =>
      if flag.isBoolean then
        setBool flag.long
        addRemaining value
      else
        setValue flag.long value
    | none => throw (.unknownFlag (String.singleton c))
  | .positional value => parsePositional value
  | .endOfFlags => pure ()

/-- Parse all tokens -/
partial def parseAll : ParserM Unit := do
  match ← advance with
  | some token =>
    parseToken token
    parseAll
  | none => pure ()

/-- Apply default values for missing flags/args -/
def applyDefaults : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Apply flag defaults
  for flag in cmd.flags do
    if let some dflt := flag.defaultValue then
      if state.values.getValue flag.long |>.isNone then
        if !state.values.hasBool flag.long then
          setValue flag.long dflt

  -- Apply arg defaults
  for arg in cmd.args do
    if let some dflt := arg.defaultValue then
      if state.values.getValue arg.name |>.isNone then
        setValue arg.name dflt

/-- Validate required flags and arguments -/
def validateRequired : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Check required flags
  for flag in cmd.flags do
    if flag.required then
      if state.values.getValue flag.long |>.isNone then
        if !state.values.hasBool flag.long then
          throw (.missingRequired s!"--{flag.long}")

  -- Check required args
  for arg in cmd.args do
    if arg.required then
      if state.values.getValue arg.name |>.isNone then
        throw (.missingRequired arg.name)

/-- Build the final result -/
def buildResult : ParserM ParseResult := do
  let state ← get
  pure {
    commandPath := state.commandPath
    values := state.values
    remaining := state.remaining
  }

end ParserM

/-- Parse command-line arguments against a command definition -/
def parse (cmd : Command) (args : List String) : Except ParseError ParseResult := do
  let tokens := tokenize args
  let initialState : ParserState := {
    tokens := tokens
    currentCommand := cmd
  }

  let action := do
    ParserM.parseAll
    ParserM.applyDefaults
    ParserM.validateRequired
    ParserM.buildResult

  let (result, _) := action.run initialState
  result

end Parlance.Parse

namespace Parlance

/-- Top-level parse function -/
def parse (cmd : Command) (args : List String) : Except ParseError ParseResult :=
  Parse.parse cmd args

end Parlance
