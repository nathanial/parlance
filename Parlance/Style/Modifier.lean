/-
  Parlance.Style.Modifier - Text modifiers (bold, italic, etc.)
-/

import Parlance.Style.Ansi

namespace Parlance.Style

/-- Text modifiers (attributes) -/
structure Modifier where
  bold : Bool := false
  dim : Bool := false
  italic : Bool := false
  underline : Bool := false
  blink : Bool := false
  reverse : Bool := false
  hidden : Bool := false
  strikethrough : Bool := false
  deriving Repr, BEq, Inhabited

namespace Modifier

def empty : Modifier := {}

def mkBold : Modifier := { bold := true }
def mkDim : Modifier := { dim := true }
def mkItalic : Modifier := { italic := true }
def mkUnderline : Modifier := { underline := true }
def mkBlink : Modifier := { blink := true }
def mkReverse : Modifier := { reverse := true }
def mkHidden : Modifier := { hidden := true }
def mkStrikethrough : Modifier := { strikethrough := true }

/-- Merge two modifiers (OR operation) -/
def merge (m1 m2 : Modifier) : Modifier := {
  bold := m1.bold || m2.bold
  dim := m1.dim || m2.dim
  italic := m1.italic || m2.italic
  underline := m1.underline || m2.underline
  blink := m1.blink || m2.blink
  reverse := m1.reverse || m2.reverse
  hidden := m1.hidden || m2.hidden
  strikethrough := m1.strikethrough || m2.strikethrough
}

/-- Check if any modifiers are set -/
def isEmpty (m : Modifier) : Bool :=
  !m.bold && !m.dim && !m.italic && !m.underline &&
  !m.blink && !m.reverse && !m.hidden && !m.strikethrough

/-- Convert to list of ANSI codes -/
def toCodes (m : Modifier) : List String :=
  let codes := []
  let codes := if m.bold then codes ++ [Ansi.codeBold] else codes
  let codes := if m.dim then codes ++ [Ansi.codeDim] else codes
  let codes := if m.italic then codes ++ [Ansi.codeItalic] else codes
  let codes := if m.underline then codes ++ [Ansi.codeUnderline] else codes
  let codes := if m.blink then codes ++ [Ansi.codeBlink] else codes
  let codes := if m.reverse then codes ++ [Ansi.codeReverse] else codes
  let codes := if m.hidden then codes ++ [Ansi.codeHidden] else codes
  let codes := if m.strikethrough then codes ++ [Ansi.codeStrikethrough] else codes
  codes

instance : Append Modifier where
  append := merge

end Modifier

end Parlance.Style
