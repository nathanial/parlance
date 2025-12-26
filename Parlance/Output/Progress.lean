/-
  Parlance.Output.Progress - Progress bar for long-running operations
-/

import Parlance.Style.Ansi
import Parlance.Style.Styled

namespace Parlance.Output

open Style

/-- Progress bar configuration -/
structure ProgressConfig where
  /-- Width of the bar in characters -/
  width : Nat := 40
  /-- Character for filled portion -/
  filledChar : Char := '█'
  /-- Character for empty portion -/
  emptyChar : Char := '░'
  /-- Left bracket character -/
  leftBracket : String := ""
  /-- Right bracket character -/
  rightBracket : String := ""
  /-- Show percentage -/
  showPercent : Bool := true
  /-- Show count (current/total) -/
  showCount : Bool := false
  /-- Style for filled portion -/
  filledStyle : Style := { fg := .green }
  /-- Style for empty portion -/
  emptyStyle : Style := { fg := .ansi .brightBlack }
  deriving Inhabited

/-- Progress bar state -/
structure Progress where
  /-- Current progress value -/
  current : Nat
  /-- Total value -/
  total : Nat
  /-- Optional message -/
  message : String := ""
  /-- Configuration -/
  config : ProgressConfig := {}
  deriving Inhabited

namespace Progress

/-- Create a new progress bar -/
def new (total : Nat) (message : String := "") : Progress :=
  { current := 0, total, message }

/-- Create with custom config -/
def withConfig (p : Progress) (config : ProgressConfig) : Progress :=
  { p with config := config }

/-- Calculate percentage (0-100) -/
def percent (p : Progress) : Nat :=
  if p.total > 0 then (p.current * 100) / p.total else 0

/-- Render the progress bar to a string -/
def render (p : Progress) : String :=
  let pct := p.percent
  let filled := (pct * p.config.width) / 100
  let empty := p.config.width - filled

  let filledStr := String.mk (List.replicate filled p.config.filledChar)
  let emptyStr := String.mk (List.replicate empty p.config.emptyChar)

  let styledFilled := StyledText.styled filledStr p.config.filledStyle
  let styledEmpty := StyledText.styled emptyStr p.config.emptyStyle

  let bar := s!"{p.config.leftBracket}{styledFilled.render}{styledEmpty.render}{p.config.rightBracket}"

  let pctStr := if p.config.showPercent then s!" {pct}%" else ""
  let countStr := if p.config.showCount then s!" ({p.current}/{p.total})" else ""
  let msgStr := if p.message.isEmpty then "" else s!" {p.message}"

  s!"{Ansi.cr}{Ansi.clearToEndOfLine}{bar}{pctStr}{countStr}{msgStr}"

/-- Update progress and display -/
def update (p : Progress) (current : Nat) : IO Progress := do
  let p' := { p with current := current }
  IO.print p'.render
  (← IO.getStdout).flush
  pure p'

/-- Increment progress by 1 -/
def inc (p : Progress) : IO Progress :=
  p.update (p.current + 1)

/-- Increment progress by n -/
def incBy (p : Progress) (n : Nat) : IO Progress :=
  p.update (p.current + n)

/-- Set progress to a specific value -/
def set (p : Progress) (value : Nat) : IO Progress :=
  p.update value

/-- Complete the progress bar -/
def done (message : String := "Done") : IO Unit := do
  IO.print s!"{Ansi.cr}{Ansi.clearToEndOfLine}"
  let checkmark := StyledText.styled "✓" { fg := .green }
  IO.println s!"{checkmark.render} {message}"

/-- Complete with a custom styled message -/
def complete (p : Progress) : IO Unit := do
  let _ ← p.update p.total
  IO.println ""

end Progress

/-- Simple progress counter (no bar, just numbers) -/
structure Counter where
  current : Nat
  total : Nat
  message : String := ""
  deriving Inhabited

namespace Counter

def new (total : Nat) (message : String := "") : Counter :=
  { current := 0, total, message }

def render (c : Counter) : String :=
  let msgStr := if c.message.isEmpty then "" else s!"{c.message}: "
  s!"{Ansi.cr}{Ansi.clearToEndOfLine}{msgStr}{c.current}/{c.total}"

def update (c : Counter) (current : Nat) : IO Counter := do
  let c' := { c with current := current }
  IO.print c'.render
  (← IO.getStdout).flush
  pure c'

def inc (c : Counter) : IO Counter :=
  c.update (c.current + 1)

def done : IO Unit := do
  IO.println ""

end Counter

end Parlance.Output
