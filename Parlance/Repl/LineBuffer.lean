/-
  Parlance.Repl.LineBuffer - Line editing state and operations

  Manages the current input line with cursor position.
  Supports standard readline-style editing operations.
-/
import Parlance.Style.Ansi

namespace Parlance.Repl

/-- A line buffer with content and cursor position -/
structure LineBuffer where
  content : String := ""
  cursor : Nat := 0
  deriving Repr, BEq, Inhabited

namespace LineBuffer

/-- Create an empty line buffer -/
def empty : LineBuffer := {}

/-- Create a line buffer with initial content -/
def ofString (s : String) : LineBuffer :=
  { content := s, cursor := s.length }

/-- Check if the buffer is empty -/
def isEmpty (lb : LineBuffer) : Bool := lb.content.isEmpty

/-- Get the length of the content -/
def length (lb : LineBuffer) : Nat := lb.content.length

/-- Text before the cursor -/
def beforeCursor (lb : LineBuffer) : String :=
  lb.content.take lb.cursor

/-- Text after the cursor -/
def afterCursor (lb : LineBuffer) : String :=
  lb.content.drop lb.cursor

/-- Character at cursor (if any) -/
def atCursor (lb : LineBuffer) : Option Char :=
  lb.content.get? ⟨lb.cursor⟩

-- ============ Character Operations ============

/-- Insert a character at the cursor position -/
def insertChar (lb : LineBuffer) (c : Char) : LineBuffer :=
  { content := lb.beforeCursor ++ c.toString ++ lb.afterCursor
    cursor := lb.cursor + 1 }

/-- Insert a string at the cursor position -/
def insertString (lb : LineBuffer) (s : String) : LineBuffer :=
  { content := lb.beforeCursor ++ s ++ lb.afterCursor
    cursor := lb.cursor + s.length }

/-- Delete character before cursor (backspace) -/
def deleteBackward (lb : LineBuffer) : LineBuffer :=
  if lb.cursor == 0 then lb
  else
    { content := lb.content.take (lb.cursor - 1) ++ lb.afterCursor
      cursor := lb.cursor - 1 }

/-- Delete character at cursor (delete key) -/
def deleteForward (lb : LineBuffer) : LineBuffer :=
  if lb.cursor >= lb.content.length then lb
  else
    { content := lb.beforeCursor ++ lb.content.drop (lb.cursor + 1)
      cursor := lb.cursor }

-- ============ Cursor Movement ============

/-- Move cursor left one character -/
def moveCursorLeft (lb : LineBuffer) : LineBuffer :=
  if lb.cursor == 0 then lb
  else { lb with cursor := lb.cursor - 1 }

/-- Move cursor right one character -/
def moveCursorRight (lb : LineBuffer) : LineBuffer :=
  if lb.cursor >= lb.content.length then lb
  else { lb with cursor := lb.cursor + 1 }

/-- Move cursor to start of line (Ctrl+A / Home) -/
def moveCursorStart (lb : LineBuffer) : LineBuffer :=
  { lb with cursor := 0 }

/-- Move cursor to end of line (Ctrl+E / End) -/
def moveCursorEnd (lb : LineBuffer) : LineBuffer :=
  { lb with cursor := lb.content.length }

/-- Find the start of the previous word -/
private def findWordStart (s : String) (pos : Nat) : Nat :=
  if pos == 0 then 0
  else
    -- Skip trailing spaces
    let pos' := Id.run do
      let mut p := pos - 1
      while p > 0 && (s.get? ⟨p⟩).map Char.isWhitespace == some true do
        p := p - 1
      pure (if p == 0 then 0 else p)
    -- Find start of word
    Id.run do
      let mut p := pos'
      while p > 0 && (s.get? ⟨p - 1⟩).map Char.isWhitespace != some true do
        p := p - 1
      pure p

/-- Find the end of the next word -/
private def findWordEnd (s : String) (pos : Nat) : Nat :=
  let len := s.length
  if pos >= len then len
  else
    -- Skip leading spaces
    let pos' := Id.run do
      let mut p := pos
      while p < len && (s.get? ⟨p⟩).map Char.isWhitespace == some true do
        p := p + 1
      pure p
    -- Find end of word
    Id.run do
      let mut p := pos'
      while p < len && (s.get? ⟨p⟩).map Char.isWhitespace != some true do
        p := p + 1
      pure p

/-- Move cursor to start of previous word (Alt+B) -/
def moveCursorWordLeft (lb : LineBuffer) : LineBuffer :=
  { lb with cursor := findWordStart lb.content lb.cursor }

/-- Move cursor to end of next word (Alt+F) -/
def moveCursorWordRight (lb : LineBuffer) : LineBuffer :=
  { lb with cursor := findWordEnd lb.content lb.cursor }

-- ============ Deletion Operations ============

/-- Delete from cursor to end of line (Ctrl+K) -/
def deleteToEnd (lb : LineBuffer) : LineBuffer :=
  { lb with content := lb.beforeCursor }

/-- Delete from start of line to cursor (Ctrl+U) -/
def deleteToStart (lb : LineBuffer) : LineBuffer :=
  { content := lb.afterCursor, cursor := 0 }

/-- Delete the previous word (Ctrl+W) -/
def deleteWord (lb : LineBuffer) : LineBuffer :=
  let wordStart := findWordStart lb.content lb.cursor
  { content := lb.content.take wordStart ++ lb.afterCursor
    cursor := wordStart }

/-- Clear the entire line -/
def clear (lb : LineBuffer) : LineBuffer := empty

-- ============ Content Operations ============

/-- Replace all content (preserving cursor if possible) -/
def setContent (lb : LineBuffer) (s : String) : LineBuffer :=
  { content := s, cursor := min lb.cursor s.length }

/-- Get the content as a string -/
def toString (lb : LineBuffer) : String := lb.content

-- ============ Rendering ============

/-- Render the line with prompt for display.
    Returns ANSI string that:
    1. Moves to start of line
    2. Clears to end
    3. Prints prompt + content
    4. Positions cursor correctly -/
def render (lb : LineBuffer) (prompt : String) : String :=
  let output := Parlance.Style.Ansi.cr ++
                Parlance.Style.Ansi.clearToEndOfLine ++
                prompt ++ lb.content
  -- Position cursor: move back from end to cursor position
  let backCount := lb.content.length - lb.cursor
  if backCount > 0 then
    output ++ Parlance.Style.Ansi.cursorBack backCount
  else
    output

end LineBuffer

end Parlance.Repl
