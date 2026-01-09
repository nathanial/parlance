/-
  Parlance.Repl - Interactive REPL support

  Provides readline-style line editing for building interactive CLI applications.

  ## Quick Start

  ```lean
  import Parlance.Repl

  def main : IO Unit := do
    Parlance.Repl.simple ">>> " fun input => do
      if input == "quit" then
        pure true  -- Exit
      else
        IO.println s!"You said: {input}"
        pure false -- Continue
  ```

  ## Features

  - Line editing with cursor movement (arrows, Home/End)
  - Standard editing shortcuts:
    - Ctrl+A/E: Start/end of line
    - Ctrl+K: Delete to end of line
    - Ctrl+U: Delete to start of line
    - Ctrl+W: Delete previous word
    - Ctrl+L: Clear screen
    - Alt+B/F: Move by word
  - Command history with up/down arrows
  - Reverse history search with Ctrl+R
  - Tab completion with customizable providers
  - Persistent history file support
  - Ctrl+C: Clear line / interrupt
  - Ctrl+D: Exit (on empty line)
-/

import Parlance.Repl.Terminal
import Parlance.Repl.Input
import Parlance.Repl.LineBuffer
import Parlance.Repl.History
import Parlance.Repl.Completion
import Parlance.Repl.Core
