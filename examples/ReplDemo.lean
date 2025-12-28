/-
  REPL Demo - Simple interactive shell example

  Run with: lake exe repl_demo
-/
import Parlance.Repl

def main : IO Unit := do
  IO.println "Parlance REPL Demo"
  IO.println "Type 'help' for commands, 'quit' to exit"
  IO.println ""

  Parlance.Repl.simple "demo> " fun input => do
    match input.trim.toLower with
    | "quit" | "exit" | "q" =>
      IO.println "Goodbye!"
      pure true  -- Exit
    | "help" | "?" =>
      IO.println "Commands:"
      IO.println "  help  - Show this help"
      IO.println "  echo  - Echo the rest of the line"
      IO.println "  quit  - Exit the REPL"
      IO.println ""
      IO.println "Editing shortcuts:"
      IO.println "  Ctrl+A/E  - Start/end of line"
      IO.println "  Ctrl+K    - Delete to end"
      IO.println "  Ctrl+U    - Delete to start"
      IO.println "  Ctrl+W    - Delete word"
      IO.println "  Ctrl+L    - Clear screen"
      IO.println "  Alt+B/F   - Move by word"
      pure false
    | cmd =>
      if cmd.startsWith "echo " then
        IO.println (cmd.drop 5)
      else if cmd.isEmpty then
        pure ()
      else
        IO.println s!"Unknown command: {cmd}"
      pure false
