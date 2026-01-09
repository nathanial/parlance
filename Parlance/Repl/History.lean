/-
  Parlance.Repl.History - Command history with navigation and persistence

  Provides:
  - In-memory history buffer with up/down navigation
  - Reverse search (Ctrl+R style)
  - File persistence for history across sessions
-/

namespace Parlance.Repl

/-- Command history with navigation state -/
structure History where
  /-- All history entries (oldest first) -/
  entries : Array String := #[]
  /-- Current position during navigation. entries.size means "at prompt" (not navigating) -/
  position : Nat := 0
  /-- Maximum number of entries to keep -/
  maxEntries : Nat := 1000
  /-- Saved current line when starting navigation (restored on down past end) -/
  tempLine : String := ""
  /-- Whether we're in reverse search mode -/
  searchMode : Bool := false
  /-- Current search query in reverse search mode -/
  searchQuery : String := ""
  /-- Last search result index (for continuing search) -/
  searchIndex : Option Nat := none
  deriving Repr, Inhabited

namespace History

/-- Create an empty history with the given max entries -/
def empty (maxEntries : Nat := 1000) : History :=
  { maxEntries }

/-- Check if currently navigating history (not at prompt) -/
def isNavigating (h : History) : Bool :=
  h.position < h.entries.size

/-- Get the current entry (if navigating) -/
def current (h : History) : Option String :=
  if h.position < h.entries.size then some h.entries[h.position]! else none

/-- Reset navigation to the end (at prompt) -/
def resetPosition (h : History) : History :=
  { h with position := h.entries.size, tempLine := "", searchMode := false, searchQuery := "", searchIndex := none }

/-- Check if line matches last entry (for duplicate suppression) -/
private def matchesLast (h : History) (line : String) : Bool :=
  if h.entries.size > 0 then
    h.entries[h.entries.size - 1]! == line
  else
    false

/-- Add an entry to history. Resets navigation position.
    Skips empty lines and consecutive duplicates. -/
def add (h : History) (line : String) : History :=
  -- Skip empty lines
  if line.trim.isEmpty then
    h.resetPosition
  -- Skip consecutive duplicates
  else if matchesLast h line then
    h.resetPosition
  else
    let newEntries := h.entries.push line
    -- Trim to max size (remove oldest entries)
    let trimmed := if newEntries.size > h.maxEntries then
      newEntries.extract (newEntries.size - h.maxEntries) newEntries.size
    else
      newEntries
    { h with entries := trimmed, position := trimmed.size, tempLine := "" }

/-- Navigate to previous entry (up arrow).
    Returns the new history state and the line to display.
    First call saves the current line as tempLine. -/
def previous (h : History) (currentLine : String) : History × Option String :=
  if h.entries.isEmpty then
    (h, none)
  else if h.position == h.entries.size then
    -- First up: save current line, go to last entry
    let newPos := h.entries.size - 1
    let newH := { h with position := newPos, tempLine := currentLine }
    (newH, some h.entries[newPos]!)
  else if h.position > 0 then
    -- Already navigating: go to previous
    let newPos := h.position - 1
    let newH := { h with position := newPos }
    (newH, some h.entries[newPos]!)
  else
    -- At oldest entry, can't go further
    (h, none)

/-- Navigate to next entry (down arrow).
    Returns the new history state and the line to display.
    Going past the end restores tempLine. -/
def next (h : History) : History × Option String :=
  if h.position >= h.entries.size then
    -- Already at prompt, nothing to do
    (h, none)
  else if h.position == h.entries.size - 1 then
    -- At last entry, go back to prompt (restore tempLine)
    let newH := { h with position := h.entries.size }
    (newH, some h.tempLine)
  else
    -- Navigate to next entry
    let newPos := h.position + 1
    let newH := { h with position := newPos }
    (newH, some h.entries[newPos]!)

/-- Check if a string contains a substring -/
private def containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Search backward for an entry containing the query.
    Returns the matching entry if found. -/
def searchBackward (h : History) (query : String) : History × Option String :=
  if query.isEmpty then
    (h, none)
  else
    -- Start from current position - 1 (or end if not searching yet)
    let startPos := match h.searchIndex with
      | some idx => if idx > 0 then idx - 1 else h.entries.size
      | none => if h.entries.size > 0 then h.entries.size - 1 else 0

    -- Search backward for a match using iteration
    let result := Id.run do
      let mut pos := startPos
      while pos < h.entries.size do
        let entry := h.entries[pos]!
        if containsSubstr entry query then
          return some pos
        if pos == 0 then
          return none
        pos := pos - 1
      return none

    match result with
    | some idx =>
      let newH := { h with searchMode := true, searchQuery := query, searchIndex := some idx, position := idx }
      (newH, some h.entries[idx]!)
    | none =>
      ({ h with searchMode := true, searchQuery := query }, none)

/-- Exit search mode and keep the current selection -/
def exitSearchMode (h : History) : History :=
  { h with searchMode := false, searchQuery := "", searchIndex := none }

/-- Enter search mode -/
def enterSearchMode (h : History) : History :=
  { h with searchMode := true, searchQuery := "", searchIndex := none }

-- ============ Persistence ============

/-- Load history from a file. Returns empty history if file doesn't exist. -/
def load (path : System.FilePath) (maxEntries : Nat := 1000) : IO History := do
  try
    let contents ← IO.FS.readFile path
    let lines := contents.splitOn "\n" |>.filter (·.length > 0)
    -- Take only the last maxEntries lines
    let entries := if lines.length > maxEntries then
      lines.drop (lines.length - maxEntries) |>.toArray
    else
      lines.toArray
    pure { entries, position := entries.size, maxEntries }
  catch _ =>
    -- File doesn't exist or can't be read
    pure (empty maxEntries)

/-- Save all history to a file (overwrites existing) -/
def save (h : History) (path : System.FilePath) : IO Unit := do
  let contents := h.entries.toList.foldl (· ++ · ++ "\n") ""
  IO.FS.writeFile path contents

/-- Append a single line to the history file (for incremental saving) -/
def appendLine (path : System.FilePath) (line : String) : IO Unit := do
  try
    let handle ← IO.FS.Handle.mk path .append
    handle.putStrLn line
    handle.flush
  catch _ =>
    -- Create file if it doesn't exist
    IO.FS.writeFile path (line ++ "\n")

end History

end Parlance.Repl
