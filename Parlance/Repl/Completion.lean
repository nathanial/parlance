/-
  Parlance.Repl.Completion - Tab completion for REPL

  Provides:
  - Completion provider interface
  - Built-in completers (path, static words)
  - Completion state for cycling through candidates
-/
import Parlance.Core.Types
import Parlance.Style.Ansi

namespace Parlance.Repl

/-- A completion candidate -/
structure Completion where
  /-- The text to insert when this completion is selected -/
  text : String
  /-- Display text (can be different from text, e.g., with styling) -/
  display : String := text
  /-- Optional description shown after the completion -/
  description : String := ""
  deriving Repr, BEq, Inhabited

namespace Completion

/-- Create a simple completion with just text -/
def simple (text : String) : Completion := { text }

/-- Create a completion with description -/
def withDesc (text : String) (desc : String) : Completion :=
  { text, description := desc }

end Completion

/-- Completion provider function type.
    Takes the current line and cursor position.
    Returns an array of completions. -/
def CompletionProvider := String → Nat → IO (Array Completion)

/-- State for completion cycling -/
structure CompletionState where
  /-- Available completion candidates -/
  candidates : Array Completion := #[]
  /-- Currently selected index -/
  index : Nat := 0
  /-- The word being completed (prefix before cursor) -/
  wordPrefix : String := ""
  /-- Start position of the word being completed -/
  startPos : Nat := 0
  /-- Original line before any completion was applied -/
  originalLine : String := ""
  /-- Original cursor position -/
  originalCursor : Nat := 0
  deriving Repr, Inhabited

namespace CompletionState

/-- Check if we're currently completing -/
def isActive (cs : CompletionState) : Bool :=
  !cs.candidates.isEmpty

/-- Get the currently selected completion -/
def current (cs : CompletionState) : Option Completion :=
  if cs.index < cs.candidates.size then some cs.candidates[cs.index]! else none

/-- Move to the next completion (wrap around) -/
def next (cs : CompletionState) : CompletionState :=
  if cs.candidates.isEmpty then cs
  else { cs with index := (cs.index + 1) % cs.candidates.size }

/-- Move to the previous completion (wrap around) -/
def prev (cs : CompletionState) : CompletionState :=
  if cs.candidates.isEmpty then cs
  else
    let newIdx := if cs.index == 0 then cs.candidates.size - 1 else cs.index - 1
    { cs with index := newIdx }

/-- Clear completion state -/
def clear : CompletionState := {}

/-- Apply the current completion to the line.
    Returns the new line and cursor position. -/
def apply (cs : CompletionState) : Option (String × Nat) :=
  match cs.current with
  | none => none
  | some c =>
    -- Replace the prefix with the completion text
    let before := cs.originalLine.take cs.startPos
    let after := cs.originalLine.drop cs.originalCursor
    let newLine := before ++ c.text ++ after
    let newCursor := cs.startPos + c.text.length
    some (newLine, newCursor)

end CompletionState

namespace Completers

/-- Find the word being completed (working backward from cursor) -/
def findWordStart (line : String) (cursor : Nat) : Nat := Id.run do
  let mut pos := cursor
  while pos > 0 do
    let charPos := pos - 1
    if charPos < line.length then
      let c := line.get ⟨charPos⟩
      if c.isWhitespace then
        return pos
    pos := pos - 1
  return 0

/-- Extract the word prefix being completed -/
def getPrefix (line : String) (cursor : Nat) : String × Nat :=
  let start := findWordStart line cursor
  (line.extract ⟨start⟩ ⟨cursor⟩, start)

/-- Static word completer - matches from a fixed list of words -/
def staticCompleter (words : Array String) : CompletionProvider := fun line cursor => do
  let (pref, _) := getPrefix line cursor
  if pref.isEmpty then
    pure #[]
  else
    let matched := words.filter (·.startsWith pref)
    pure (matched.map Completion.simple)

/-- Path completer - completes file and directory paths -/
def pathCompleter : CompletionProvider := fun line cursor => do
  let (pref, _) := getPrefix line cursor
  if pref.isEmpty then
    pure #[]
  else
    try
      -- Parse the path
      let path := System.FilePath.mk pref
      let (dir, filePrefix) :=
        if pref.endsWith "/" then
          (path, "")
        else
          let parent := path.parent.getD (System.FilePath.mk ".")
          let fileName := path.fileName.getD ""
          (parent, fileName)

      -- List directory contents
      let entries ← System.FilePath.readDir dir
      let matched := entries.filter (fun e => e.fileName.startsWith filePrefix)
        |>.map (fun e =>
          let fullPath := if pref.endsWith "/" then
            pref ++ e.fileName
          else
            (dir / e.fileName).toString
          Completion.simple fullPath)
      pure matched
    catch _ =>
      pure #[]

/-- Command completer - completes from Parlance Command subcommands and flags -/
def commandCompleter (cmd : Command) : CompletionProvider := fun line cursor => do
  let (pref, _) := getPrefix line cursor
  let words := line.take cursor |>.splitOn " " |>.filter (·.length > 0)

  -- Determine what to complete based on context
  if pref.startsWith "--" then
    -- Complete long flags
    let flagPrefix := pref.drop 2
    let matched := cmd.flags.toList
      |>.filter (·.long.startsWith flagPrefix)
      |>.map (fun f => Completion.withDesc s!"--{f.long}" f.description)
    pure matched.toArray
  else if pref.startsWith "-" && pref.length == 2 then
    -- Complete short flags - show all short flags
    let matched := cmd.flags.toList
      |>.filterMap (fun f => f.short.map (fun c => Completion.withDesc s!"-{c}" f.description))
    pure matched.toArray
  else if words.isEmpty || (words.length == 1 && !pref.isEmpty) then
    -- Complete subcommands at start
    let matched := cmd.subcommands.toList
      |>.filter (·.name.startsWith pref)
      |>.map (fun c => Completion.withDesc c.name c.description)
    pure matched.toArray
  else
    pure #[]

/-- Combine multiple completers -/
def combineCompleters (completers : Array CompletionProvider) : CompletionProvider := fun line cursor => do
  let mut results := #[]
  for completer in completers do
    let completions ← completer line cursor
    results := results ++ completions
  pure results

end Completers

-- ============ Display Helpers ============

/-- Format completions for display below the prompt -/
def formatCompletions (completions : Array Completion) (selected : Nat) (_maxWidth : Nat := 80) : String :=
  if completions.isEmpty then ""
  else
    let lines := completions.mapIdx fun idx c =>
      let marker := if idx == selected then "> " else "  "
      let desc := if c.description.isEmpty then "" else s!" -- {c.description}"
      marker ++ c.display ++ desc
    -- Join with newlines, limit to reasonable height
    let displayed := lines.toList.take 10
    "\n" ++ String.intercalate "\n" displayed

/-- Get common prefix of all completions (for partial completion) -/
def commonPrefix (completions : Array Completion) : String :=
  if completions.isEmpty then ""
  else
    let first := completions[0]!.text
    completions.foldl (init := first) fun acc c =>
      let minLen := min acc.length c.text.length
      -- Find common prefix length
      let commonLen := Id.run do
        let mut pos := 0
        while pos < minLen do
          if acc.get ⟨pos⟩ != c.text.get ⟨pos⟩ then
            return pos
          pos := pos + 1
        return minLen
      acc.take commonLen

end Parlance.Repl
