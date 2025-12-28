/-
  Parlance.Wrap - Word wrapping for terminal output

  Wraps text at word boundaries without splitting words.
  Handles ANSI escape codes correctly (doesn't count them toward line width).
-/

namespace Parlance.Wrap

/-- State for streaming word wrapper -/
structure State where
  maxWidth : Nat := 80      -- Maximum line width
  linePos : Nat := 0        -- Current visual position on line
  wordBuffer : String := "" -- Current word being accumulated
  wordVisualLen : Nat := 0  -- Visual length of word (excluding ANSI codes)
  inEscape : Bool := false  -- Currently inside an ANSI escape sequence
  deriving Repr, Inhabited

def State.new (maxWidth : Nat := 80) : State :=
  { maxWidth := maxWidth }

/-- Check if we're starting an ANSI escape sequence -/
def isEscapeStart (c : Char) : Bool := c == '\x1b'

/-- Process a single character, updating state and returning output -/
def step (s : State) (c : Char) : State × String := Id.run do
  -- Handle ANSI escape sequences
  if s.inEscape then
    -- Still in escape sequence, add to word buffer but don't count width
    let newBuffer := s.wordBuffer.push c
    -- Escape sequences end with a letter (typically 'm' for SGR)
    let stillInEscape := !c.isAlpha
    return ({ s with wordBuffer := newBuffer, inEscape := stillInEscape }, "")

  if isEscapeStart c then
    -- Starting an escape sequence
    return ({ s with wordBuffer := s.wordBuffer.push c, inEscape := true }, "")

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
