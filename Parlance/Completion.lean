/-
  Parlance.Completion - Shell completion script generation

  This module provides automatic generation of shell completion scripts
  for Bash, Zsh, and Fish shells based on your command definition.

  Usage:
  ```lean
  def main (args : List String) : IO UInt32 := do
    let cmd := command "myapp" do
      Cmd.boolFlag "verbose" (short := some 'v')
      Cmd.flag "format" (argType := .choice ["json", "yaml"])

    -- Handle completion request before normal parsing
    if let some action := Completion.handleCompletionRequest cmd "myapp" args then
      action
      return 0

    -- Normal parsing continues...
  ```

  Users can then run:
  ```bash
  myapp --generate-completion bash > ~/.bash_completion.d/myapp
  myapp --generate-completion zsh > ~/.zsh/completions/_myapp
  myapp --generate-completion fish > ~/.config/fish/completions/myapp.fish
  ```
-/

import Parlance.Completion.Core
import Parlance.Completion.Bash
import Parlance.Completion.Zsh
import Parlance.Completion.Fish

namespace Parlance.Completion

open Parlance

/-- Generate a completion script for the specified shell -/
def generateScript (cmd : Command) (binaryName : String) (shell : Shell) : String :=
  match shell with
  | .bash => Bash.generateScript cmd binaryName
  | .zsh => Zsh.generateScript cmd binaryName
  | .fish => Fish.generateScript cmd binaryName

/-- Check if args contain --generate-completion and handle it.
    Returns `some action` if completion was requested, `none` otherwise.

    This function looks for `--generate-completion <shell>` in the argument list
    and returns an IO action that prints the completion script if found.

    Usage in main:
    ```lean
    def main (args : List String) : IO UInt32 := do
      let cmd := command "myapp" do ...

      if let some action := handleCompletionRequest cmd "myapp" args then
        action
        return 0

      -- Normal parsing continues...
    ```
-/
def handleCompletionRequest (cmd : Command) (binaryName : String) (args : List String)
    : Option (IO Unit) := do
  -- Look for --generate-completion <shell>
  let idx ← args.findIdx? (· == "--generate-completion")
  let shellArg ← args[idx + 1]?
  let shell ← Shell.fromString shellArg
  some (IO.println (generateScript cmd binaryName shell))

/-- Usage text for the --generate-completion flag (for inclusion in help text) -/
def completionUsage : String :=
  "--generate-completion <SHELL>  Generate shell completion script (bash, zsh, fish)"

/-- Extended usage text with installation instructions -/
def completionHelp (binaryName : String) : String :=
  s!"Shell Completion:
  {binaryName} --generate-completion bash   Generate Bash completions
  {binaryName} --generate-completion zsh    Generate Zsh completions
  {binaryName} --generate-completion fish   Generate Fish completions

Installation:
  Bash:  {binaryName} --generate-completion bash > ~/.bash_completion.d/{binaryName}
  Zsh:   {binaryName} --generate-completion zsh > ~/.zsh/completions/_{binaryName}
  Fish:  {binaryName} --generate-completion fish > ~/.config/fish/completions/{binaryName}.fish"

end Parlance.Completion
