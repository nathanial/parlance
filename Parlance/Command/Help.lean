/-
  Parlance.Command.Help - Automatic help text generation
-/

import Parlance.Core.Types
import Parlance.Style.Styled
import Parlance.Style.Semantic

namespace Parlance

open Style

/-- Configuration for help text generation -/
structure HelpConfig where
  /-- Width of the help text (for wrapping) -/
  width : Nat := 80
  /-- Indent for descriptions -/
  indent : Nat := 2
  /-- Column where descriptions start -/
  descColumn : Nat := 24
  /-- Use colors in output -/
  useColors : Bool := true
  /-- Show full details for subcommands (args and flags) -/
  showSubcommandDetails : Bool := true
  deriving Inhabited

/-- Format a flag for help display -/
private def formatFlag (f : Flag) (config : HelpConfig) : String :=
  let shortStr := match f.short with
    | some c => s!"-{c}, "
    | none => "    "
  let longStr := if f.isBoolean && f.negatable then s!"--[no-]{f.long}" else s!"--{f.long}"
  let typeStr := match f.argType with
    | some t => s!" <{t}>"
    | none => ""
  let pref := s!"{shortStr}{longStr}{typeStr}"
  let padding := if pref.length < config.descColumn
    then String.mk (List.replicate (config.descColumn - pref.length) ' ')
    else ""
  let paddedPref := pref ++ padding
  -- Add repeatable indicator
  let repeatStr := if f.repeatable then " [can repeat]" else ""
  -- Add environment variable info to description
  let envStr := match f.envVar with
    | some env => s!" [env: {env}]"
    | none => ""
  let fullDesc := f.description ++ repeatStr ++ envStr
  if fullDesc.isEmpty then
    s!"  {pref}"
  else if pref.length >= config.descColumn then
    s!"  {pref}\n{String.mk (List.replicate (config.descColumn + 2) ' ')}{fullDesc}"
  else
    s!"  {paddedPref}{fullDesc}"

/-- Format a flag with custom indentation (for subcommand details) -/
private def formatFlagIndented (f : Flag) (config : HelpConfig) (indent : Nat) : String :=
  let indentStr := String.mk (List.replicate indent ' ')
  let shortStr := match f.short with
    | some c => s!"-{c}, "
    | none => "    "
  let longStr := if f.isBoolean && f.negatable then s!"--[no-]{f.long}" else s!"--{f.long}"
  -- Simplify type display: show <VALUE> for choices, otherwise show type
  let typeStr := match f.argType with
    | some (.choice _) => " <VALUE>"
    | some t => s!" <{t}>"
    | none => ""
  let pref := s!"{shortStr}{longStr}{typeStr}"
  -- Use wider column for subcommand flags
  let targetCol := config.descColumn + 4
  let padding := if pref.length < targetCol
    then String.mk (List.replicate (targetCol - pref.length) ' ')
    else "  "
  s!"{indentStr}{pref}{padding}{f.description}"

/-- Format a positional argument for help display -/
private def formatArg (a : Arg) (config : HelpConfig) : String :=
  let reqStr := if a.required then "" else " (optional)"
  let nameStr := s!"<{a.name}>"
  let pref := nameStr
  let padding := if pref.length < config.descColumn
    then String.mk (List.replicate (config.descColumn - pref.length) ' ')
    else ""
  let paddedPref := pref ++ padding
  if a.description.isEmpty then
    s!"  {pref}{reqStr}"
  else if pref.length >= config.descColumn then
    s!"  {pref}\n{String.mk (List.replicate (config.descColumn + 2) ' ')}{a.description}{reqStr}"
  else
    s!"  {paddedPref}{a.description}{reqStr}"

/-- Format a subcommand for help display -/
private def formatSubcmd (c : Command) (config : HelpConfig) : String :=
  let pref := c.name
  let padding := if pref.length < config.descColumn
    then String.mk (List.replicate (config.descColumn - pref.length) ' ')
    else ""
  let paddedPref := pref ++ padding
  if c.description.isEmpty then
    s!"  {pref}"
  else if pref.length >= config.descColumn then
    s!"  {pref}\n{String.mk (List.replicate (config.descColumn + 2) ' ')}{c.description}"
  else
    s!"  {paddedPref}{c.description}"

/-- Format a subcommand with full details (args and flags) -/
private def formatSubcmdFull (c : Command) (config : HelpConfig) : String :=
  -- Build usage: "add <title> [comment]"
  let argStr := c.args.toList.map (fun a =>
    if a.required then s!"<{a.name}>" else s!"[{a.name}]"
  ) |> " ".intercalate
  let nameWithArgs := if argStr.isEmpty then c.name else s!"{c.name} {argStr}"

  -- First line: name with args and description
  let padding := if nameWithArgs.length < config.descColumn
    then String.mk (List.replicate (config.descColumn - nameWithArgs.length) ' ')
    else " "
  let headerLine := s!"  {nameWithArgs}{padding}{c.description}"

  -- Flag lines (indented by 4)
  let flagLines := c.flags.toList.map (formatFlagIndented · config 4)

  if flagLines.isEmpty then
    headerLine
  else
    "\n".intercalate ([headerLine] ++ flagLines)

/-- Generate usage string for a command -/
def Command.usage (cmd : Command) (programName : Option String := none) : String :=
  let name := programName.getD cmd.name
  let flagStr := if cmd.flags.isEmpty then "" else " [OPTIONS]"
  let subcmdStr := if cmd.subcommands.isEmpty then "" else " <COMMAND>"
  let argStr := cmd.args.toList.map (fun a =>
    if a.required then s!"<{a.name}>" else s!"[{a.name}]"
  ) |> " ".intercalate
  let argPart := if argStr.isEmpty then "" else s!" {argStr}"
  s!"{name}{flagStr}{subcmdStr}{argPart}"

/-- Generate full help text for a command -/
def Command.helpText (cmd : Command) (config : HelpConfig := {}) : String :=
  -- Description section
  let descLines :=
    if cmd.description.isEmpty then []
    else [cmd.description, ""]

  -- Usage section
  let usageLines := [s!"Usage: {cmd.usage}", ""]

  -- Arguments section
  let argsLines :=
    if cmd.args.isEmpty then []
    else
      let formatted := cmd.args.toList.map (formatArg · config)
      ["Arguments:"] ++ formatted ++ [""]

  -- Options section
  let optsLines :=
    if cmd.flags.isEmpty then []
    else
      let formatted := cmd.flags.toList.map (formatFlag · config)
      let helpPadding := String.mk (List.replicate (config.descColumn - 12) ' ')
      let helpLine := s!"  -h, --help{helpPadding}Print help"
      let versionLine :=
        if cmd.version.isEmpty then []
        else
          let vPadding := String.mk (List.replicate (config.descColumn - 15) ' ')
          [s!"  -V, --version{vPadding}Print version"]
      ["Options:"] ++ formatted ++ [helpLine] ++ versionLine ++ [""]

  -- Commands section
  let cmdsLines :=
    if cmd.subcommands.isEmpty then []
    else if config.showSubcommandDetails then
      -- Full details: join with blank lines for readability
      let formatted := cmd.subcommands.toList.map (formatSubcmdFull · config)
      ["Commands:", "\n\n".intercalate formatted, ""]
    else
      let formatted := cmd.subcommands.toList.map (formatSubcmd · config)
      ["Commands:"] ++ formatted ++ [""]

  "\n".intercalate (descLines ++ usageLines ++ argsLines ++ optsLines ++ cmdsLines)

/-- Print help text to stdout -/
def Command.printHelp (cmd : Command) (config : HelpConfig := {}) : IO Unit :=
  IO.println (cmd.helpText config)

/-- Print version to stdout -/
def Command.printVersion (cmd : Command) : IO Unit := do
  if cmd.version.isEmpty then
    IO.println s!"{cmd.name}"
  else
    IO.println s!"{cmd.name} {cmd.version}"

/-- Generate help text for a specific subcommand path -/
def Command.helpTextForPath (cmd : Command) (path : List String) (config : HelpConfig := {}) : Option String :=
  match path with
  | [] => some (cmd.helpText config)
  | name :: rest =>
    cmd.findSubcommand name >>= fun subcmd =>
      subcmd.helpTextForPath rest config

end Parlance
