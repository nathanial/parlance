/-
  Parlance.Repl.Core - REPL configuration and main loop

  Provides the core REPL functionality:
  - Configuration for prompts, history, completion
  - Single-line input with editing
  - Main run loop
-/
import Parlance.Repl.Terminal
import Parlance.Repl.Input
import Parlance.Repl.LineBuffer
import Parlance.Style.Ansi

namespace Parlance.Repl

open Terminal Input LineBuffer

/-- REPL configuration -/
structure ReplConfig where
  /-- Function to generate the prompt (can be dynamic) -/
  prompt : IO String := pure "> "
  /-- Called when Ctrl+C is pressed. Return true to exit, false to continue. -/
  onInterrupt : IO Bool := pure false
  deriving Inhabited

/-- Internal state during line reading -/
private structure ReadState where
  buffer : LineBuffer := LineBuffer.empty
  promptStr : String := ""
  deriving Inhabited

/-- Refresh the display after a buffer change -/
private def refreshDisplay (state : ReadState) : IO Unit := do
  IO.print (state.buffer.render state.promptStr)
  (← IO.getStdout).flush

/-- Handle a key event during line reading.
    Returns (new buffer, should submit, should exit) -/
private def handleKey (state : ReadState) (key : KeyEvent) : IO (ReadState × Bool × Bool) := do
  let buf := state.buffer
  match key.code, key.modifiers.ctrl, key.modifiers.alt with
  -- Submit line
  | .enter, false, false =>
    IO.println ""  -- Move to next line
    pure (state, true, false)

  -- Exit on Ctrl+D (only if line is empty)
  | .char 'd', true, false =>
    if buf.isEmpty then
      IO.println ""
      pure (state, false, true)
    else
      -- Ctrl+D with content: delete forward (like delete key)
      let newBuf := buf.deleteForward
      let newState := { state with buffer := newBuf }
      refreshDisplay newState
      pure (newState, false, false)

  -- Ctrl+C: interrupt
  | .char 'c', true, false =>
    IO.println "^C"
    pure ({ state with buffer := LineBuffer.empty }, false, false)

  -- Character insertion
  | .char c, false, false =>
    let newBuf := buf.insertChar c
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  | .space, false, false =>
    let newBuf := buf.insertChar ' '
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Backspace
  | .backspace, _, _ =>
    let newBuf := buf.deleteBackward
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Delete
  | .delete, _, _ =>
    let newBuf := buf.deleteForward
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Cursor movement
  | .left, false, false =>
    let newBuf := buf.moveCursorLeft
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  | .right, false, false =>
    let newBuf := buf.moveCursorRight
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  | .home, _, _ =>
    let newBuf := buf.moveCursorStart
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  | .«end», _, _ =>
    let newBuf := buf.moveCursorEnd
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+A: start of line
  | .char 'a', true, false =>
    let newBuf := buf.moveCursorStart
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+E: end of line
  | .char 'e', true, false =>
    let newBuf := buf.moveCursorEnd
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+K: delete to end of line
  | .char 'k', true, false =>
    let newBuf := buf.deleteToEnd
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+U: delete to start of line
  | .char 'u', true, false =>
    let newBuf := buf.deleteToStart
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+W: delete previous word
  | .char 'w', true, false =>
    let newBuf := buf.deleteWord
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ctrl+L: clear screen and redraw
  | .char 'l', true, false =>
    IO.print "\x1b[2J\x1b[H"  -- Clear screen and move to top-left
    refreshDisplay state
    pure (state, false, false)

  -- Alt+B: move word left
  | .char 'b', false, true =>
    let newBuf := buf.moveCursorWordLeft
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Alt+F: move word right
  | .char 'f', false, true =>
    let newBuf := buf.moveCursorWordRight
    let newState := { state with buffer := newBuf }
    refreshDisplay newState
    pure (newState, false, false)

  -- Ignore other keys
  | _, _, _ => pure (state, false, false)

/-- Read a single line with editing support.
    Returns none on EOF (Ctrl+D on empty line), some content otherwise. -/
partial def readLine (config : ReplConfig := {}) : IO (Option String) := do
  let promptStr ← config.prompt
  IO.print promptStr
  (← IO.getStdout).flush

  let initialState : ReadState := { promptStr }

  withRawMode do
    let rec loop (state : ReadState) : IO (Option String) := do
      let key ← Input.read
      let (newState, submit, exit) ← handleKey state key
      if exit then
        pure none
      else if submit then
        pure (some newState.buffer.content)
      else
        loop newState
    loop initialState

/-- Run an interactive REPL session.
    The handler receives each line and returns true to exit, false to continue. -/
partial def run (config : ReplConfig := {}) (handler : String → IO Bool) : IO Unit := do
  let rec loop : IO Unit := do
    match ← readLine config with
    | none =>
      -- EOF (Ctrl+D on empty line)
      pure ()
    | some line =>
      if line.isEmpty then
        loop  -- Skip empty lines
      else
        let shouldExit ← handler line
        if shouldExit then pure ()
        else loop
  loop

/-- Simple REPL with a fixed prompt string -/
def simple (prompt : String := "> ") (handler : String → IO Bool) : IO Unit :=
  run { prompt := pure prompt } handler

end Parlance.Repl
