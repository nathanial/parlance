/-
  Parlance.Completion.Core - Core types for shell completion
-/

import Parlance.Core.Types

namespace Parlance.Completion

/-- Supported shells for completion script generation -/
inductive Shell where
  | bash
  | zsh
  | fish
  deriving Repr, BEq, Inhabited

namespace Shell

/-- Parse a shell name from string (case-insensitive) -/
def fromString (s : String) : Option Shell :=
  match s.toLower with
  | "bash" => some .bash
  | "zsh" => some .zsh
  | "fish" => some .fish
  | _ => none

/-- Convert shell to string -/
def toString : Shell → String
  | .bash => "bash"
  | .zsh => "zsh"
  | .fish => "fish"

instance : ToString Shell := ⟨Shell.toString⟩

/-- All supported shells -/
def all : List Shell := [.bash, .zsh, .fish]

end Shell

/-- Escape a string for use in shell scripts (single quotes) -/
def escapeForShell (s : String) : String :=
  -- In single-quoted strings, escape single quotes as '\''
  s.replace "'" "'\\''"

/-- Escape a string for use in double-quoted shell strings -/
def escapeForDoubleQuotes (s : String) : String :=
  s.replace "\\" "\\\\"
    |>.replace "\"" "\\\""
    |>.replace "$" "\\$"
    |>.replace "`" "\\`"

/-- Generate a list of all flag completions for a command (--long and -s forms) -/
def flagCompletions (cmd : Command) : List String :=
  cmd.flags.toList.flatMap fun f =>
    let long := s!"--{f.long}"
    match f.short with
    | some c => [long, s!"-{c}"]
    | none => [long]

/-- Generate a list of all subcommand names -/
def subcommandCompletions (cmd : Command) : List String :=
  cmd.subcommands.toList.map (·.name)

/-- Check if a command has any flags -/
def hasFlags (cmd : Command) : Bool :=
  !cmd.flags.isEmpty

/-- Check if a command has any subcommands -/
def hasSubcommands (cmd : Command) : Bool :=
  !cmd.subcommands.isEmpty

/-- Get flags that take values (non-boolean flags) -/
def valuedFlags (cmd : Command) : List Flag :=
  cmd.flags.toList.filter (·.argType.isSome)

/-- Get boolean flags (no value) -/
def booleanFlags (cmd : Command) : List Flag :=
  cmd.flags.toList.filter (·.argType.isNone)

end Parlance.Completion
