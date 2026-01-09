/-
  Parlance.Repl.Core - REPL configuration and main loop

  Provides the core REPL functionality:
  - Configuration for prompts, history, completion
  - Single-line input with editing
  - History navigation with up/down arrows
  - Tab completion
  - Reverse search with Ctrl+R
  - Main run loop
-/
import Parlance.Repl.Terminal
import Parlance.Repl.Input
import Parlance.Repl.LineBuffer
import Parlance.Repl.History
import Parlance.Repl.Completion
import Parlance.Style.Ansi
import Chronicle

namespace Parlance.Repl

open Terminal Input LineBuffer

/-- REPL configuration -/
structure ReplConfig where
  /-- Function to generate the prompt (can be dynamic) -/
  prompt : IO String := pure "> "
  /-- Called when Ctrl+C is pressed. Return true to exit, false to continue. -/
  onInterrupt : IO Bool := pure false
  /-- Optional logger for REPL events -/
  logger : Option Chronicle.Logger := none
  /-- Path to history file for persistence (none = no persistence) -/
  historyFile : Option System.FilePath := none
  /-- Maximum number of history entries to keep -/
  maxHistory : Nat := 1000
  /-- Optional completion provider for tab completion -/
  completer : Option CompletionProvider := none

/-- Internal state during line reading -/
private structure ReadState where
  buffer : LineBuffer := LineBuffer.empty
  promptStr : String := ""
  history : History := History.empty
  completion : CompletionState := {}
  /-- True when in reverse search mode (Ctrl+R) -/
  searchMode : Bool := false
  /-- Current search query -/
  searchQuery : String := ""
  deriving Inhabited

/-- Refresh the display after a buffer change -/
private def refreshDisplay (state : ReadState) : IO Unit := do
  -- If in search mode, show a special prompt
  if state.searchMode then
    let searchPrompt := s!"(reverse-i-search)`{state.searchQuery}': "
    IO.print (Parlance.Style.Ansi.cr ++ Parlance.Style.Ansi.clearToEndOfLine ++
              searchPrompt ++ state.buffer.content)
    (← IO.getStdout).flush
  else if state.completion.isActive then
    -- Show current line plus completions below
    IO.print (state.buffer.render state.promptStr)
    IO.print (formatCompletions state.completion.candidates state.completion.index)
    -- Move cursor back to the right position
    let linesDown := min state.completion.candidates.size 10
    IO.print (Parlance.Style.Ansi.cursorUp linesDown)
    IO.print (Parlance.Style.Ansi.cr)
    IO.print (state.buffer.render state.promptStr)
    (← IO.getStdout).flush
  else
    IO.print (state.buffer.render state.promptStr)
    (← IO.getStdout).flush

/-- Clear completion display (erase the lines showing completions) -/
private def clearCompletionDisplay (state : ReadState) : IO Unit := do
  if state.completion.isActive then
    let linesDown := min state.completion.candidates.size 10
    -- Move down and clear each line
    for _ in [:linesDown] do
      IO.print "\n"
      IO.print Parlance.Style.Ansi.clearToEndOfLine
    -- Move back up
    IO.print (Parlance.Style.Ansi.cursorUp linesDown)

/-- Handle a key event during line reading.
    Returns (new state, should submit, should exit) -/
private partial def handleKey (state : ReadState) (key : KeyEvent) (config : ReplConfig) : IO (ReadState × Bool × Bool) := do
  let buf := state.buffer

  -- Special handling for search mode
  if state.searchMode then
    match key.code, key.modifiers.ctrl with
    -- Escape: exit search mode
    | .escape, _ =>
      let newState := { state with searchMode := false, searchQuery := "" }
      refreshDisplay newState
      pure (newState, false, false)
    -- Enter: accept current match and exit search
    | .enter, _ =>
      let hist' := state.history.exitSearchMode
      let newState := { state with searchMode := false, searchQuery := "", history := hist' }
      IO.println ""
      pure (newState, true, false)
    -- Ctrl+R again: search for next match
    | .char 'r', true =>
      let (newHist, matchOpt) := state.history.searchBackward state.searchQuery
      let newBuf := match matchOpt with
        | some m => LineBuffer.ofString m
        | none => state.buffer
      let newState := { state with history := newHist, buffer := newBuf }
      refreshDisplay newState
      pure (newState, false, false)
    -- Ctrl+G or Ctrl+C: cancel search
    | .char 'g', true | .char 'c', true =>
      let hist' := state.history.resetPosition
      let buf' := LineBuffer.ofString state.history.tempLine
      let newState := { state with searchMode := false, searchQuery := "", history := hist', buffer := buf' }
      refreshDisplay newState
      pure (newState, false, false)
    -- Backspace: remove from search query
    | .backspace, _ =>
      if state.searchQuery.isEmpty then
        pure (state, false, false)
      else
        let newQuery := state.searchQuery.dropRight 1
        let (newHist, matchOpt) := state.history.searchBackward newQuery
        let newBuf := match matchOpt with
          | some m => LineBuffer.ofString m
          | none => state.buffer
        let newState := { state with searchQuery := newQuery, history := newHist, buffer := newBuf }
        refreshDisplay newState
        pure (newState, false, false)
    -- Character: add to search query
    | .char c, false =>
      let newQuery := state.searchQuery.push c
      let (newHist, matchOpt) := state.history.searchBackward newQuery
      let newBuf := match matchOpt with
        | some m => LineBuffer.ofString m
        | none => state.buffer
      let newState := { state with searchQuery := newQuery, history := newHist, buffer := newBuf }
      refreshDisplay newState
      pure (newState, false, false)
    | _, _ =>
      -- Any other key exits search mode and processes the key
      let hist' := state.history.exitSearchMode
      let newState := { state with searchMode := false, searchQuery := "", history := hist' }
      handleKey newState key config

  -- If completion is active, handle tab differently
  else if state.completion.isActive then
    match key.code with
    -- Tab: cycle to next completion
    | .tab =>
      let newCompl := state.completion.next
      match newCompl.apply with
      | some (newLine, newCursor) =>
        let newBuf := { content := newLine, cursor := newCursor }
        let newState := { state with buffer := newBuf, completion := newCompl }
        refreshDisplay newState
        pure (newState, false, false)
      | none =>
        pure (state, false, false)
    -- Escape: cancel completion
    | .escape =>
      clearCompletionDisplay state
      let newBuf := { content := state.completion.originalLine, cursor := state.completion.originalCursor }
      let newState := { state with buffer := newBuf, completion := CompletionState.clear }
      refreshDisplay newState
      pure (newState, false, false)
    -- Enter: accept completion and submit
    | .enter =>
      clearCompletionDisplay state
      let hist' := state.history.resetPosition
      let newState := { state with completion := CompletionState.clear, history := hist' }
      IO.println ""
      pure (newState, true, false)
    -- Any other key: accept completion and process the key
    | _ =>
      clearCompletionDisplay state
      let newState := { state with completion := CompletionState.clear }
      handleKey newState key config

  else
    match key.code, key.modifiers.ctrl, key.modifiers.alt with
    -- Submit line
    | .enter, false, false =>
      let newState := { state with history := state.history.resetPosition }
      IO.println ""  -- Move to next line
      pure (newState, true, false)

    -- Exit on Ctrl+D (only if line is empty)
    | .char 'd', true, false =>
      if buf.isEmpty then
        IO.println ""
        pure (state, false, true)
      else
        -- Ctrl+D with content: delete forward (like delete key)
        let newBuf := buf.deleteForward
        let newState := { state with buffer := newBuf, history := state.history.resetPosition }
        refreshDisplay newState
        pure (newState, false, false)

    -- Ctrl+C: interrupt
    | .char 'c', true, false =>
      IO.println "^C"
      pure ({ state with buffer := LineBuffer.empty, history := state.history.resetPosition }, false, false)

    -- Ctrl+R: enter reverse search mode
    | .char 'r', true, false =>
      let newHist := state.history.enterSearchMode
      let hist' := { newHist with tempLine := buf.content }
      let newState := { state with searchMode := true, searchQuery := "", history := hist' }
      refreshDisplay newState
      pure (newState, false, false)

    -- Up arrow: previous history entry
    | .up, false, false =>
      let (newHist, lineOpt) := state.history.previous buf.content
      match lineOpt with
      | some line =>
        let newBuf := LineBuffer.ofString line
        let newState := { state with buffer := newBuf, history := newHist }
        refreshDisplay newState
        pure (newState, false, false)
      | none =>
        pure (state, false, false)

    -- Down arrow: next history entry
    | .down, false, false =>
      let (newHist, lineOpt) := state.history.next
      match lineOpt with
      | some line =>
        let newBuf := LineBuffer.ofString line
        let newState := { state with buffer := newBuf, history := newHist }
        refreshDisplay newState
        pure (newState, false, false)
      | none =>
        pure (state, false, false)

    -- Tab: trigger completion
    | .tab, false, false =>
      match config.completer with
      | none => pure (state, false, false)
      | some completer =>
        let completions ← completer buf.content buf.cursor
        if completions.isEmpty then
          pure (state, false, false)
        else if completions.size == 1 then
          -- Single completion: insert directly
          let (_, startPos) := Completers.getPrefix buf.content buf.cursor
          let completion := completions[0]!
          let before := buf.content.take startPos
          let after := buf.content.drop buf.cursor
          let newLine := before ++ completion.text ++ after
          let newCursor := startPos + completion.text.length
          let newBuf := { content := newLine, cursor := newCursor }
          let newState := { state with buffer := newBuf }
          refreshDisplay newState
          pure (newState, false, false)
        else
          -- Multiple completions: enter completion mode
          let (pref, startPos) := Completers.getPrefix buf.content buf.cursor
          let compState : CompletionState := {
            candidates := completions
            index := 0
            wordPrefix := pref
            startPos := startPos
            originalLine := buf.content
            originalCursor := buf.cursor
          }
          -- Apply first completion
          match compState.apply with
          | some (newLine, newCursor) =>
            let newBuf := { content := newLine, cursor := newCursor }
            let newState := { state with buffer := newBuf, completion := compState }
            refreshDisplay newState
            pure (newState, false, false)
          | none =>
            pure (state, false, false)

    -- Character insertion
    | .char c, false, false =>
      let newBuf := buf.insertChar c
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
      refreshDisplay newState
      pure (newState, false, false)

    | .space, false, false =>
      let newBuf := buf.insertChar ' '
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
      refreshDisplay newState
      pure (newState, false, false)

    -- Backspace
    | .backspace, _, _ =>
      let newBuf := buf.deleteBackward
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
      refreshDisplay newState
      pure (newState, false, false)

    -- Delete
    | .delete, _, _ =>
      let newBuf := buf.deleteForward
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
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
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
      refreshDisplay newState
      pure (newState, false, false)

    -- Ctrl+U: delete to start of line
    | .char 'u', true, false =>
      let newBuf := buf.deleteToStart
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
      refreshDisplay newState
      pure (newState, false, false)

    -- Ctrl+W: delete previous word
    | .char 'w', true, false =>
      let newBuf := buf.deleteWord
      let newState := { state with buffer := newBuf, history := state.history.resetPosition }
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
    Returns (content, updated history) on success, none on EOF. -/
partial def readLineWithHistory (config : ReplConfig := {}) (history : History) : IO (Option (String × History)) := do
  let promptStr ← config.prompt
  IO.print promptStr
  (← IO.getStdout).flush

  let initialState : ReadState := { promptStr, history }

  withRawMode do
    let rec loop (state : ReadState) : IO (Option (String × History)) := do
      let key ← Input.read
      let (newState, submit, exit) ← handleKey state key config
      if exit then
        pure none
      else if submit then
        let line := newState.buffer.content
        let newHist := newState.history.add line
        pure (some (line, newHist))
      else
        loop newState
    loop initialState

/-- Read a single line with editing support.
    Returns none on EOF (Ctrl+D on empty line), some content otherwise. -/
partial def readLine (config : ReplConfig := {}) : IO (Option String) := do
  match ← readLineWithHistory config (History.empty config.maxHistory) with
  | none => pure none
  | some (line, _) => pure (some line)

/-- Run an interactive REPL session.
    The handler receives each line and returns true to exit, false to continue. -/
partial def run (config : ReplConfig := {}) (handler : String → IO Bool) : IO Unit := do
  -- Log REPL start
  if let some logger := config.logger then
    logger.debug "REPL session started"

  -- Load history if configured
  let history ← match config.historyFile with
    | some path => History.load path config.maxHistory
    | none => pure (History.empty config.maxHistory)

  let rec loop (hist : History) : IO Unit := do
    match ← readLineWithHistory config hist with
    | none =>
      -- EOF (Ctrl+D on empty line)
      if let some logger := config.logger then
        logger.debug "REPL session ended (EOF)"
      pure ()
    | some (line, newHist) =>
      if line.isEmpty then
        loop newHist  -- Skip empty lines (history already updated appropriately)
      else
        -- Persist history entry if configured
        if let some path := config.historyFile then
          History.appendLine path line

        -- Log input (at trace level to avoid logging sensitive data at higher levels)
        if let some logger := config.logger then
          if line.startsWith "/" then
            logger.debug s!"REPL command: {line}"
          else
            logger.trace s!"REPL input: {line.length} chars"

        let shouldExit ← handler line
        if shouldExit then
          if let some logger := config.logger then
            logger.debug "REPL session ended (handler exit)"
          pure ()
        else loop newHist
  loop history

/-- Simple REPL with a fixed prompt string -/
def simple (prompt : String := "> ") (logger : Option Chronicle.Logger := none) (handler : String → IO Bool) : IO Unit :=
  run { prompt := pure prompt, logger := logger } handler

/-- REPL with history file -/
def withHistory (prompt : String := "> ") (historyFile : System.FilePath)
    (handler : String → IO Bool) : IO Unit :=
  run { prompt := pure prompt, historyFile := some historyFile } handler

/-- REPL with completion -/
def withCompletion (prompt : String := "> ") (completer : CompletionProvider)
    (handler : String → IO Bool) : IO Unit :=
  run { prompt := pure prompt, completer := some completer } handler

end Parlance.Repl
