/-
  Parlance.Repl.Input - Key event types and input parsing

  Simplified from terminus for REPL-specific needs:
  - Arrow keys, home/end, delete for line editing
  - Ctrl keys for editing shortcuts
  - No mouse events, no function keys
-/
import Parlance.Repl.Terminal

namespace Parlance.Repl

/-- Key modifiers (Ctrl, Alt) -/
structure KeyModifiers where
  ctrl : Bool := false
  alt : Bool := false
  deriving Repr, BEq, Inhabited

namespace KeyModifiers

def none : KeyModifiers := {}
def mkCtrl : KeyModifiers := { ctrl := true }
def mkAlt : KeyModifiers := { alt := true }

end KeyModifiers

/-- Keyboard key codes for REPL input -/
inductive KeyCode where
  | char (c : Char)
  | enter
  | escape
  | backspace
  | tab
  | space
  | up
  | down
  | left
  | right
  | home
  | «end»
  | delete
  | unknown
  deriving Repr, BEq, Inhabited

namespace KeyCode

def isChar : KeyCode → Bool
  | char _ => true
  | _ => false

def toChar : KeyCode → Option Char
  | char c => some c
  | _ => none

end KeyCode

/-- A keyboard event with key code and modifiers -/
structure KeyEvent where
  code : KeyCode
  modifiers : KeyModifiers := {}
  deriving Repr, BEq, Inhabited

namespace KeyEvent

def char (c : Char) : KeyEvent := { code := .char c }
def enter : KeyEvent := { code := .enter }
def escape : KeyEvent := { code := .escape }
def backspace : KeyEvent := { code := .backspace }
def tab : KeyEvent := { code := .tab }
def up : KeyEvent := { code := .up }
def down : KeyEvent := { code := .down }
def left : KeyEvent := { code := .left }
def right : KeyEvent := { code := .right }

def withCtrl (e : KeyEvent) : KeyEvent := { e with modifiers.ctrl := true }
def withAlt (e : KeyEvent) : KeyEvent := { e with modifiers.alt := true }

/-- Check if this is Ctrl+C -/
def isCtrlC (e : KeyEvent) : Bool :=
  e.modifiers.ctrl && e.code == .char 'c'

/-- Check if this is Ctrl+D -/
def isCtrlD (e : KeyEvent) : Bool :=
  e.modifiers.ctrl && e.code == .char 'd'

end KeyEvent

namespace Input

open Terminal

/-- Parse an escape sequence (after reading ESC byte) -/
private def parseEscapeSequence : IO KeyEvent := do
  match ← readByte with
  | none => pure KeyEvent.escape
  | some 91 => -- '[' - CSI sequence
    match ← readByte with
    | none => pure KeyEvent.escape
    | some b =>
      match b with
      | 65 => pure KeyEvent.up      -- A
      | 66 => pure KeyEvent.down    -- B
      | 67 => pure { code := .right } -- C
      | 68 => pure { code := .left }  -- D
      | 72 => pure { code := .home }  -- H
      | 70 => pure { code := .«end» } -- F
      | 49 => -- '1' - Home (some terminals)
        discard readByte -- consume ~
        pure { code := .home }
      | 51 => -- '3' - Delete
        discard readByte -- consume ~
        pure { code := .delete }
      | 52 => -- '4' - End
        discard readByte -- consume ~
        pure { code := .«end» }
      | _ => pure KeyEvent.escape
  | some 79 => -- 'O' - SS3 sequence (some terminals use this for arrows)
    match ← readByte with
    | some 72 => pure { code := .home } -- H
    | some 70 => pure { code := .«end» } -- F
    | _ => pure KeyEvent.escape
  | some b =>
    -- Alt + key
    if b >= 97 && b <= 122 then -- a-z
      pure { code := .char (Char.ofNat b.toNat), modifiers := KeyModifiers.mkAlt }
    else
      pure KeyEvent.escape

/-- Parse a single byte into a KeyEvent -/
private def parseInput (b : UInt8) : IO KeyEvent := do
  match b with
  | 9 => pure KeyEvent.tab
  | 10 | 13 => pure KeyEvent.enter
  | 27 => parseEscapeSequence
  | 127 => pure KeyEvent.backspace
  | _ =>
    if b >= 1 && b <= 26 then
      -- Ctrl+A through Ctrl+Z
      let c := Char.ofNat (b.toNat + 96)
      pure { code := .char c, modifiers := KeyModifiers.mkCtrl }
    else if b == 32 then
      pure { code := .space }
    else if b >= 32 && b < 127 then
      pure (KeyEvent.char (Char.ofNat b.toNat))
    else
      pure { code := .unknown }

/-- Poll for the next key event (non-blocking, returns none if no input) -/
def poll : IO (Option KeyEvent) := do
  match ← readByte with
  | none => pure none
  | some b => some <$> parseInput b

/-- Wait for the next key event (blocking) -/
partial def read : IO KeyEvent := do
  match ← poll with
  | some ke => pure ke
  | none =>
    IO.sleep 10
    read

end Input

end Parlance.Repl
