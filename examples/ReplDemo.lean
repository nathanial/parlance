/-
  REPL Demo - Interactive shell with history and completion

  Run with: lake exe repl_demo

  Features demonstrated:
  - Command history with up/down arrows
  - Persistent history file (.repl_demo_history)
  - Tab completion for commands
  - Reverse history search with Ctrl+R
-/
import Parlance.Repl

open Parlance.Repl

/-- Available commands for completion -/
def commands : Array String := #["help", "echo", "quit", "exit", "clear", "history"]

/-- Simple completer for demo commands -/
def demoCompleter : CompletionProvider := fun line cursor => do
  let (prefix, _) := Completers.getPrefix line cursor
  if prefix.isEmpty then
    pure #[]
  else
    let matches := commands.filter (·.startsWith prefix)
    pure (matches.map Completion.simple)

def main : IO Unit := do
  IO.println "Parlance REPL Demo"
  IO.println "Type 'help' for commands, 'quit' to exit"
  IO.println ""

  let config : ReplConfig := {
    prompt := pure "demo> "
    historyFile := some ".repl_demo_history"
    maxHistory := 100
    completer := some demoCompleter
  }

  Parlance.Repl.run config fun input => do
    match input.trim.toLower with
    | "quit" | "exit" | "q" =>
      IO.println "Goodbye!"
      pure true  -- Exit
    | "help" | "?" =>
      IO.println "Commands:"
      IO.println "  help    - Show this help"
      IO.println "  echo    - Echo the rest of the line"
      IO.println "  clear   - Clear the screen"
      IO.println "  history - Show history info"
      IO.println "  quit    - Exit the REPL"
      IO.println ""
      IO.println "Features:"
      IO.println "  Up/Down   - Navigate command history"
      IO.println "  Tab       - Complete commands"
      IO.println "  Ctrl+R    - Reverse search history"
      IO.println ""
      IO.println "Editing shortcuts:"
      IO.println "  Ctrl+A/E  - Start/end of line"
      IO.println "  Ctrl+K    - Delete to end"
      IO.println "  Ctrl+U    - Delete to start"
      IO.println "  Ctrl+W    - Delete word"
      IO.println "  Ctrl+L    - Clear screen"
      IO.println "  Alt+B/F   - Move by word"
      pure false
    | "clear" =>
      IO.print "\x1b[2J\x1b[H"
      pure false
    | "history" =>
      IO.println "History is stored in .repl_demo_history"
      IO.println "Use up/down arrows to navigate, Ctrl+R to search"
      pure false
    | cmd =>
      if cmd.startsWith "echo " then
        IO.println (cmd.drop 5)
      else if cmd.isEmpty then
        pure ()
      else
        IO.println s!"Unknown command: {cmd}"
        IO.println "Type 'help' for available commands"
      pure false
