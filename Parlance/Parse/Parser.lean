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

/-- Add a value for a repeatable flag (appends without replacing) -/
def addValue (name : String) (value : String) : ParserM Unit := do
  let state ← get
  set { state with values := state.values.addValue name value }

/-- Record a boolean flag -/
def setBool (name : String) : ParserM Unit := do
  let state ← get
  set { state with values := state.values.setBool name }

/-- Record a boolean flag as false (for --no-<flag>) -/
def setBoolFalse (name : String) : ParserM Unit := do
  let state ← get
  let cleared := state.values.clearBool name
  let updated := cleared.setValue name "false"
  set { state with values := updated }

/-- Add to remaining positional args -/
def addRemaining (value : String) : ParserM Unit := do
  let state ← get
  set { state with remaining := state.remaining ++ [value] }

/-- Validate choice values against allowed options. -/
def validateChoice (name : String) (argType : ArgType) (value : String) : ParserM Unit := do
  match argType with
  | .choice options =>
    if options.contains value then
      pure ()
    else
      throw (.invalidChoice name value options)
  | _ => pure ()

/-- Validate choice values for optional arg types (flags). -/
def validateChoice? (name : String) (argType : Option ArgType) (value : String) : ParserM Unit := do
  match argType with
  | some t => validateChoice name t value
  | none => pure ()

/-- Run custom validator if present -/
def runValidator (name : String) (value : String) (validate : Option (String → Except String Unit)) : ParserM Unit := do
  match validate with
  | some f =>
    match f value with
    | .ok () => pure ()
    | .error msg => throw (.validationFailed name value msg)
  | none => pure ()

/-- Prepend tokens to be processed next -/
def prependTokens (tokens : List Token) : ParserM Unit := do
  if tokens.isEmpty then
    pure ()
  else
    modify fun state => { state with tokens := tokens ++ state.tokens }

/-- Enqueue a short-flag tail (from -abc) for parsing -/
def enqueueShortTail (tail : String) : ParserM Unit := do
  let tokens := tokenizeShortGroup tail
  prependTokens tokens

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

/-- Parse a negated long flag like --no-<flag>. -/
def parseNegatedLongFlag (name : String) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand
  if name.startsWith "no-" then
    let base := name.drop 3
    match cmd.findFlagLong base with
    | some flag =>
      if flag.isBoolean && flag.negatable then
        setBoolFalse flag.long
      else
        throw (.unknownFlag name)
    | none => throw (.unknownFlag name)
  else
    throw (.unknownFlag name)

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
      validateChoice? flag.long flag.argType value
      runValidator flag.long value flag.validate
      if flag.repeatable then
        addValue flag.long value
      else
        setValue flag.long value
  | none => parseNegatedLongFlag name

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
      validateChoice? flag.long flag.argType value
      runValidator flag.long value flag.validate
      if flag.repeatable then
        addValue flag.long value
      else
        setValue flag.long value
  | none => parseNegatedLongFlag name

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
      validateChoice? flag.long flag.argType value
      runValidator flag.long value flag.validate
      if flag.repeatable then
        addValue flag.long value
      else
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
    validateChoice arg.name arg.argType value
    runValidator arg.name value arg.validate
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

    -- Built-in flags
    if c == 'h' then
      throw .helpRequested
    if c == 'V' then
      throw .versionRequested

    match state.currentCommand.findFlagShort c with
    | some flag =>
      if flag.isBoolean then
        setBool flag.long
        enqueueShortTail value
      else
        validateChoice? flag.long flag.argType value
        runValidator flag.long value flag.validate
        if flag.repeatable then
          addValue flag.long value
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

/-- Apply default values for missing flags/args (pure version without env vars) -/
def applyDefaults : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Apply flag defaults
  for flag in cmd.flags do
    if let some dflt := flag.defaultValue then
      -- For repeatable flags, check hasValue; for others, check getValue
      let hasExisting := if flag.repeatable
        then state.values.hasValue flag.long
        else state.values.getValue flag.long |>.isSome || state.values.hasBool flag.long
      if !hasExisting then
        validateChoice? flag.long flag.argType dflt
        runValidator flag.long dflt flag.validate
        if flag.repeatable then
          addValue flag.long dflt
        else
          setValue flag.long dflt

  -- Apply arg defaults
  for arg in cmd.args do
    if let some dflt := arg.defaultValue then
      if state.values.getValue arg.name |>.isNone then
        validateChoice arg.name arg.argType dflt
        runValidator arg.name dflt arg.validate
        setValue arg.name dflt

/-- Apply defaults with environment variable lookup -/
def applyDefaultsWithEnv (getEnv : String → Option String) : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Apply environment variables and flag defaults
  for flag in cmd.flags do
    -- For repeatable flags, check hasValue; for others, check getValue
    let hasExisting := if flag.repeatable
      then state.values.hasValue flag.long
      else state.values.getValue flag.long |>.isSome || state.values.hasBool flag.long
    if !hasExisting then
      -- First try environment variable
      if let some envName := flag.envVar then
        if let some envValue := getEnv envName then
          -- For boolean flags, treat non-empty as true
          if flag.isBoolean then
            if envValue.toLower == "true" || envValue == "1" || envValue.toLower == "yes" then
              setBool flag.long
          else
            validateChoice? flag.long flag.argType envValue
            runValidator flag.long envValue flag.validate
            if flag.repeatable then
              addValue flag.long envValue
            else
              setValue flag.long envValue
          continue
      -- Then try default value
      if let some dflt := flag.defaultValue then
        validateChoice? flag.long flag.argType dflt
        runValidator flag.long dflt flag.validate
        if flag.repeatable then
          addValue flag.long dflt
        else
          setValue flag.long dflt

  -- Apply arg defaults (args don't support env vars)
  for arg in cmd.args do
    if let some dflt := arg.defaultValue then
      if state.values.getValue arg.name |>.isNone then
        validateChoice arg.name arg.argType dflt
        runValidator arg.name dflt arg.validate
        setValue arg.name dflt

/-- Validate required flags and arguments -/
def validateRequired : ParserM Unit := do
  let state ← get
  let cmd := state.currentCommand

  -- Check required flags
  for flag in cmd.flags do
    if flag.required then
      -- For repeatable flags, check hasValue; for others, check getValue
      let hasValue := if flag.repeatable
        then state.values.hasValue flag.long
        else state.values.getValue flag.long |>.isSome || state.values.hasBool flag.long
      if !hasValue then
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

/-- Top-level parse function (pure, without environment variable support) -/
def parse (cmd : Command) (args : List String) : Except ParseError ParseResult :=
  Parse.parse cmd args

/-- Parse with environment variable lookup (pass a custom getEnv function) -/
def parseWithEnv (cmd : Command) (args : List String) (getEnv : String → Option String) : Except ParseError ParseResult := do
  let tokens := Parse.tokenize args
  let initialState : Parse.ParserState := {
    tokens := tokens
    currentCommand := cmd
  }

  let action := do
    Parse.ParserM.parseAll
    Parse.ParserM.applyDefaultsWithEnv getEnv
    Parse.ParserM.validateRequired
    Parse.ParserM.buildResult

  let (result, _) := action.run initialState
  result

/-- Parse with system environment variables (IO version) -/
def parseIO (cmd : Command) (args : List String) : IO (Except ParseError ParseResult) := do
  -- Build a list of (envName, value) pairs for all defined env vars
  let mut envPairs : List (String × String) := []
  for flag in cmd.flags do
    if let some envName := flag.envVar then
      if let some value ← IO.getEnv envName then
        envPairs := (envName, value) :: envPairs
  let getEnv := fun name => envPairs.find? (·.1 == name) |>.map (·.2)
  pure (parseWithEnv cmd args getEnv)

end Parlance
