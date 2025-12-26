/-
  Parlance.Output.Spinner - Animated spinner for progress indication
-/

import Parlance.Style.Ansi
import Parlance.Style.Styled

namespace Parlance.Output

open Style

/-- Predefined spinner frame sets -/
def spinnerDots : Array String := #["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
def spinnerBraille : Array String := #["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
def spinnerLine : Array String := #["-", "\\", "|", "/"]
def spinnerCircle : Array String := #["◐", "◓", "◑", "◒"]
def spinnerSquare : Array String := #["◰", "◳", "◲", "◱"]
def spinnerArrows : Array String := #["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]
def spinnerAscii : Array String := #["[    ]", "[=   ]", "[==  ]", "[=== ]", "[ ===]", "[  ==]", "[   =]"]

/-- Spinner state -/
structure Spinner where
  /-- Animation frames -/
  frames : Array String := spinnerDots
  /-- Message to display -/
  message : String
  /-- Current frame index -/
  frameIndex : Nat := 0
  /-- Style for the spinner -/
  style : Style := { fg := .cyan }
  deriving Inhabited

namespace Spinner

/-- Create a new spinner with a message -/
def new (message : String) (frames : Array String := spinnerDots) : Spinner :=
  { frames, message }

/-- Create a spinner with a specific style -/
def withStyle (s : Spinner) (style : Style) : Spinner :=
  { s with style := style }

/-- Get the current frame -/
def currentFrame (s : Spinner) : String :=
  s.frames[s.frameIndex % s.frames.size]!

/-- Render the current spinner state -/
def render (s : Spinner) : String :=
  let frame := s.currentFrame
  let styledFrame := StyledText.styled frame s.style
  s!"{Ansi.cr}{Ansi.clearToEndOfLine}{styledFrame.render} {s.message}"

/-- Advance to the next frame and render -/
def tick (s : Spinner) : IO Spinner := do
  IO.print s.render
  (← IO.getStdout).flush
  pure { s with frameIndex := s.frameIndex + 1 }

/-- Clear the spinner line -/
def clear : IO Unit := do
  IO.print s!"{Ansi.cr}{Ansi.clearToEndOfLine}"
  (← IO.getStdout).flush

/-- Complete the spinner with a success message -/
def done (message : String) : IO Unit := do
  clear
  let checkmark := StyledText.styled "✓" { fg := .green }
  IO.println s!"{checkmark.render} {message}"

/-- Complete the spinner with a failure message -/
def fail (message : String) : IO Unit := do
  clear
  let cross := StyledText.styled "✗" { fg := .red }
  IO.println s!"{cross.render} {message}"

/-- Complete the spinner with a warning message -/
def warn (message : String) : IO Unit := do
  clear
  let warning := StyledText.styled "⚠" { fg := .yellow }
  IO.println s!"{warning.render} {message}"

end Spinner

/-- Run an IO action with a simple spinner (no animation, just start/end messages) -/
def withSpinner (message : String) (action : IO α) : IO α := do
  let spinner := Spinner.new message
  let _ ← spinner.tick
  try
    let result ← action
    Spinner.done message
    pure result
  catch e =>
    Spinner.fail s!"{message} (failed)"
    throw e

/-- Run an IO action with animated spinner -/
def withAnimatedSpinner (message : String) (interval : UInt32 := 100)
    (action : IO α) : IO α := do
  -- For a proper implementation, we'd need threading
  -- This simplified version just shows start/end
  let spinner := Spinner.new message
  let _ ← spinner.tick
  let result ← action
  Spinner.done message
  pure result

end Parlance.Output
