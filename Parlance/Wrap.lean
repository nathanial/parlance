/-
  Parlance.Wrap - Word wrapping for terminal output

  Wraps text at word boundaries without splitting words.
  Handles ANSI escape codes correctly (doesn't count them toward line width).
  Supports both CSI sequences (\x1b[...m) and OSC sequences (\x1b]...\x1b\\).
-/

namespace Parlance.Wrap

/-- Type of escape sequence we're currently in -/
inductive EscapeType where
  | none       -- Not in escape sequence
  | sawEsc     -- Saw ESC, waiting for [ or ]
  | csi        -- In CSI sequence \x1b[..., ends with alpha
  | osc        -- In OSC sequence \x1b]..., ends with ST
  | oscSawEsc  -- In OSC, saw ESC, next char might be \ for ST
  deriving Repr, BEq, Inhabited

/-- State for streaming word wrapper -/
structure State where
  maxWidth : Nat := 80      -- Maximum line width
  linePos : Nat := 0        -- Current visual position on line
  wordBuffer : String := "" -- Current word being accumulated
  wordVisualLen : Nat := 0  -- Visual length of word (excluding ANSI codes)
  escapeType : EscapeType := .none  -- Current escape sequence state
  deriving Repr, Inhabited

def State.new (maxWidth : Nat := 80) : State :=
  { maxWidth := maxWidth }

/-- Process a single character, updating state and returning output -/
def step (s : State) (c : Char) : State × String := Id.run do
  -- Handle escape sequences based on current state
  match s.escapeType with
  | .sawEsc =>
    let newBuffer := s.wordBuffer.push c
    if c == '[' then
      -- CSI sequence starting
      return ({ s with wordBuffer := newBuffer, escapeType := .csi }, "")
    else if c == ']' then
      -- OSC sequence starting
      return ({ s with wordBuffer := newBuffer, escapeType := .osc }, "")
    else
      -- Unknown escape, treat as ended
      return ({ s with wordBuffer := newBuffer, escapeType := .none }, "")

  | .csi =>
    let newBuffer := s.wordBuffer.push c
    -- CSI ends with an alpha character
    let newType := if c.isAlpha then .none else .csi
    return ({ s with wordBuffer := newBuffer, escapeType := newType }, "")

  | .osc =>
    let newBuffer := s.wordBuffer.push c
    if c == '\x1b' then
      -- Might be start of ST (\x1b\)
      return ({ s with wordBuffer := newBuffer, escapeType := .oscSawEsc }, "")
    else if c == '\x07' then
      -- BEL also terminates OSC
      return ({ s with wordBuffer := newBuffer, escapeType := .none }, "")
    else
      return ({ s with wordBuffer := newBuffer, escapeType := .osc }, "")

  | .oscSawEsc =>
    let newBuffer := s.wordBuffer.push c
    if c == '\\' then
      -- ST complete, OSC ended
      return ({ s with wordBuffer := newBuffer, escapeType := .none }, "")
    else
      -- Not ST, continue in OSC (the ESC might start a nested sequence but keep it simple)
      return ({ s with wordBuffer := newBuffer, escapeType := .osc }, "")

  | .none =>
    -- Check for escape start
    if c == '\x1b' then
      return ({ s with wordBuffer := s.wordBuffer.push c, escapeType := .sawEsc }, "")

  -- Handle whitespace - time to emit the buffered word
  if c == ' ' || c == '\t' then
    let mut output := ""
    let mut newLinePos := s.linePos

    -- Check if word fits on current line
    if s.wordBuffer.isEmpty then
      -- No word buffered, just handle the space
      if s.linePos < s.maxWidth then
        output := " "
        newLinePos := s.linePos + 1
      -- else: at max width, skip the space
    else
      -- We have a word to emit
      if s.linePos + s.wordVisualLen > s.maxWidth && s.linePos > 0 then
        -- Word doesn't fit, wrap first
        output := "\n" ++ s.wordBuffer
        newLinePos := s.wordVisualLen
      else
        -- Word fits
        output := s.wordBuffer
        newLinePos := s.linePos + s.wordVisualLen

      -- Add space after word if not at max width
      if newLinePos < s.maxWidth then
        output := output ++ " "
        newLinePos := newLinePos + 1

    return ({ s with linePos := newLinePos, wordBuffer := "", wordVisualLen := 0 }, output)

  -- Handle newline
  if c == '\n' then
    -- Emit buffered word (if any) then newline
    let output := s.wordBuffer ++ "\n"
    return ({ s with linePos := 0, wordBuffer := "", wordVisualLen := 0 }, output)

  -- Regular character - add to word buffer
  let newBuffer := s.wordBuffer.push c
  let newVisualLen := s.wordVisualLen + 1
  return ({ s with wordBuffer := newBuffer, wordVisualLen := newVisualLen }, "")

/-- Process a chunk of text -/
def feed (s : State) (chunk : String) : State × String := Id.run do
  let mut state := s
  let mut output := ""
  for c in chunk.toList do
    let (newState, newOutput) := step state c
    state := newState
    output := output ++ newOutput
  (state, output)

/-- Flush any remaining buffered content -/
def finish (s : State) : String :=
  s.wordBuffer

/-- Convenience: wrap a complete string -/
def wrap (input : String) (maxWidth : Nat := 80) : String :=
  let (state, output) := feed (State.new maxWidth) input
  output ++ finish state

end Parlance.Wrap
