/-
  Parlance.Output.Table - Formatted table output
-/

import Parlance.Style.Styled

namespace Parlance.Output

open Style

/-- Column alignment -/
inductive Align where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/-- Column definition -/
structure Column where
  /-- Column header text -/
  header : String
  /-- Alignment for this column -/
  align : Align := .left
  /-- Minimum width (0 = auto) -/
  minWidth : Nat := 0
  /-- Maximum width (0 = unlimited) -/
  maxWidth : Nat := 0
  deriving Repr, Inhabited

/-- Table border style -/
inductive BorderStyle where
  | none
  | ascii
  | unicode
  | compact
  deriving Repr, BEq, Inhabited

/-- Table configuration -/
structure TableConfig where
  /-- Show header row -/
  showHeader : Bool := true
  /-- Style for header row -/
  headerStyle : Style := Style.bold
  /-- Border style -/
  borderStyle : BorderStyle := .none
  /-- Padding between columns -/
  padding : Nat := 2
  deriving Inhabited

/-- A table with headers and data rows -/
structure Table where
  /-- Column definitions -/
  columns : Array Column
  /-- Data rows -/
  rows : Array (Array String)
  /-- Configuration -/
  config : TableConfig := {}
  deriving Inhabited

namespace Table

/-- Create a simple table from headers and rows -/
def simple (headers : List String) (rows : List (List String)) : Table := {
  columns := headers.toArray.map fun h => { header := h }
  rows := rows.toArray.map (·.toArray)
}

/-- Create a table with custom column definitions -/
def withColumns (columns : List Column) (rows : List (List String)) : Table := {
  columns := columns.toArray
  rows := rows.toArray.map (·.toArray)
}

/-- Apply configuration -/
def withConfig (t : Table) (config : TableConfig) : Table :=
  { t with config := config }

/-- Calculate column widths based on content -/
def columnWidths (t : Table) : Array Nat :=
  t.columns.mapIdx fun i col =>
    let headerWidth := col.header.length
    let dataWidth := t.rows.foldl (init := 0) fun acc row =>
      Nat.max acc (row[i]?.map (·.length) |>.getD 0)
    let contentWidth := Nat.max headerWidth dataWidth
    let minBounded := Nat.max col.minWidth contentWidth
    if col.maxWidth > 0 then Nat.min col.maxWidth minBounded else minBounded

/-- Pad a string to a given width with alignment -/
def pad (s : String) (width : Nat) (align : Align) : String :=
  let len := s.length
  if len >= width then s.take width
  else
    let padding := width - len
    match align with
    | .left => s ++ String.mk (List.replicate padding ' ')
    | .right => String.mk (List.replicate padding ' ') ++ s
    | .center =>
      let leftPad := padding / 2
      let rightPad := padding - leftPad
      String.mk (List.replicate leftPad ' ') ++ s ++ String.mk (List.replicate rightPad ' ')

/-- Render a row as a string -/
private def renderRow (t : Table) (widths : Array Nat) (cells : Array String)
    (style : Option Style := none) : String :=
  let paddedCells := cells.zip widths |>.mapIdx fun i (cell, w) =>
    let align := t.columns[i]?.map (·.align) |>.getD .left
    pad cell w align
  let separator := String.mk (List.replicate t.config.padding ' ')
  let row := separator.intercalate paddedCells.toList
  match style with
  | some s => (StyledText.styled row s).render
  | none => row

/-- Render a separator line -/
private def renderSeparator (t : Table) (widths : Array Nat) : String :=
  match t.config.borderStyle with
  | .none => ""
  | .ascii =>
    let dashes := widths.map fun w => String.mk (List.replicate w '-')
    let separator := String.mk (List.replicate t.config.padding '-')
    separator.intercalate dashes.toList
  | .unicode =>
    let dashes := widths.map fun w => String.mk (List.replicate w '─')
    let separator := String.mk (List.replicate t.config.padding '─')
    separator.intercalate dashes.toList
  | .compact => ""

/-- Render the table to a string -/
def render (t : Table) : String :=
  let widths := t.columnWidths

  -- Header lines
  let headerLines :=
    if t.config.showHeader then
      let headers := t.columns.map (·.header)
      let headerRow := renderRow t widths headers (some t.config.headerStyle)
      let sep := renderSeparator t widths
      if sep.isEmpty then [headerRow] else [headerRow, sep]
    else []

  -- Data rows
  let dataLines := t.rows.toList.map fun row => renderRow t widths row

  "\n".intercalate (headerLines ++ dataLines)

/-- Print the table to stdout -/
def print (t : Table) : IO Unit :=
  IO.println t.render

/-- Create a key-value table (two columns) -/
def keyValue (pairs : List (String × String)) : Table :=
  simple ["Key", "Value"] (pairs.map fun (k, v) => [k, v])
  |>.withConfig { showHeader := false, padding := 2 }

end Table

/-- Print a list of items as a simple bulleted list -/
def printList (items : List String) (bullet : String := "•") : IO Unit := do
  for item in items do
    IO.println s!"{bullet} {item}"

/-- Print key-value pairs -/
def printKeyValue (pairs : List (String × String)) (separator : String := ": ") : IO Unit := do
  let maxKeyLen := pairs.foldl (init := 0) fun acc (k, _) => Nat.max acc k.length
  for (k, v) in pairs do
    let paddedKey := k ++ String.mk (List.replicate (maxKeyLen - k.length) ' ')
    IO.println s!"{paddedKey}{separator}{v}"

end Parlance.Output
