/-
  Parlance.Markdown - Streaming terminal markdown renderer

  Converts markdown syntax to styled terminal output using ANSI codes.
  Designed for streaming use where content arrives in chunks.

  Supported syntax:
  - **bold** → bold text
  - *italic* and _italic_ → italic text
  - `code` → colored inline code
  - # Header through ###### → styled headers
  - [text](url) → clickable underlined blue link (OSC 8 hyperlinks)
-/

import Parlance.Style

namespace Parlance.Markdown

open Parlance.Style

/-- Parser mode for tracking what we're currently parsing -/
inductive Mode where
  | normal           -- Regular text
  | sawStar          -- Saw single *, might be italic or start of bold
  | inBold           -- Inside **..., collecting bold content
  | sawStarInBold    -- Inside bold, saw single *, might be end or nested italic
  | inItalicStar     -- Inside *..., collecting italic content
  | inItalicUnder    -- Inside _..., collecting italic content
  | sawUnder         -- Saw single _, might start italic
  | inCode           -- Inside `..., collecting code content
  | sawHash (count : Nat) -- Saw # at line start, counting
  | inHeader (level : Nat) -- Inside header, collecting content
  | inLinkText       -- Inside [..., collecting link text
  | sawLinkClose     -- Saw ], expecting (
  | inLinkUrl        -- Inside ](..., collecting URL
  deriving Repr, BEq, Inhabited

/-- Parser state for streaming markdown -/
structure State where
  mode : Mode := .normal
  buffer : String := ""      -- Content being accumulated for current token
  linkText : String := ""    -- Link text saved while parsing URL
  atLineStart : Bool := true -- Are we at the start of a line?
  deriving Repr, Inhabited

namespace State

def new : State := {}

end State

-- Styles for different markdown elements
def boldStyle : Style := Style.bold
def italicStyle : Style := Style.italic
def boldItalicStyle : Style := Style.bold.withItalic
def codeStyle : Style := { fg := .ansi .cyan }
def linkStyle : Style := { fg := .ansi .blue, modifier := Modifier.mkUnderline }

def headerStyle (level : Nat) : Style :=
  match level with
  | 1 => { fg := .ansi .magenta, modifier := Modifier.mkBold }
  | 2 => { fg := .ansi .blue, modifier := Modifier.mkBold }
  | 3 => { fg := .ansi .cyan, modifier := Modifier.mkBold }
  | _ => Style.bold

/-- Render text with a style -/
def styled (text : String) (style : Style) : String :=
  (StyledText.styled text style).render

/-- Process a single character and update state.
    Returns (new state, output to emit) -/
def step (s : State) (c : Char) : State × String := Id.run do
  let atLineStart := s.atLineStart && c != '\n'
  let newLineStart := c == '\n'

  match s.mode with
  | .normal =>
    if c == '*' then
      ({ s with mode := .sawStar, atLineStart := newLineStart }, "")
    else if c == '_' then
      ({ s with mode := .sawUnder, atLineStart := newLineStart }, "")
    else if c == '`' then
      ({ s with mode := .inCode, buffer := "", atLineStart := newLineStart }, "")
    else if c == '#' && s.atLineStart then
      ({ s with mode := .sawHash 1, atLineStart := false }, "")
    else if c == '[' then
      ({ s with mode := .inLinkText, buffer := "", atLineStart := newLineStart }, "")
    else
      ({ s with atLineStart := newLineStart }, c.toString)

  | .sawStar =>
    if c == '*' then
      -- ** = start of bold
      ({ s with mode := .inBold, buffer := "", atLineStart := newLineStart }, "")
    else if c == '\n' then
      -- * followed by newline, emit the * literally
      ({ s with mode := .normal, atLineStart := true }, "*\n")
    else if c == ' ' then
      -- * followed by space, list marker - replace bullet with indent
      ({ s with mode := .normal, atLineStart := newLineStart }, "  ")
    else
      -- Single * = start of italic
      ({ s with mode := .inItalicStar, buffer := c.toString, atLineStart := newLineStart }, "")

  | .inBold =>
    if c == '*' then
      ({ s with mode := .sawStarInBold, atLineStart := newLineStart }, "")
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

  | .sawStarInBold =>
    if c == '*' then
      -- ** = end of bold
      let output := styled s.buffer boldStyle
      ({ s with mode := .normal, buffer := "", atLineStart := newLineStart }, output)
    else
      -- Single * in bold, keep collecting (could be nested italic, but keep simple)
      ({ s with mode := .inBold, buffer := s.buffer.push '*' |>.push c, atLineStart := newLineStart }, "")

  | .inItalicStar =>
    if c == '*' then
      -- * = end of italic
      let output := styled s.buffer italicStyle
      ({ s with mode := .normal, buffer := "", atLineStart := newLineStart }, output)
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

  | .sawUnder =>
    if c == '_' then
      -- __ could be bold underscore style, but keep simple - emit literally
      ({ s with mode := .normal, atLineStart := newLineStart }, "__")
    else if c == '\n' then
      ({ s with mode := .normal, atLineStart := true }, "_\n")
    else if c == ' ' then
      -- _ followed by space, probably not italic
      ({ s with mode := .normal, atLineStart := newLineStart }, "_ ")
    else
      -- Start of italic
      ({ s with mode := .inItalicUnder, buffer := c.toString, atLineStart := newLineStart }, "")

  | .inItalicUnder =>
    if c == '_' then
      -- _ = end of italic
      let output := styled s.buffer italicStyle
      ({ s with mode := .normal, buffer := "", atLineStart := newLineStart }, output)
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

  | .inCode =>
    if c == '`' then
      -- End of inline code
      let output := styled s.buffer codeStyle
      ({ s with mode := .normal, buffer := "", atLineStart := newLineStart }, output)
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

  | .sawHash count =>
    if c == '#' && count < 6 then
      ({ s with mode := .sawHash (count + 1), atLineStart := false }, "")
    else if c == ' ' then
      -- Space after hashes = header
      ({ s with mode := .inHeader count, buffer := "", atLineStart := false }, "")
    else if c == '\n' then
      -- Hashes followed by newline, emit literally
      let hashes := String.mk (List.replicate count '#')
      ({ s with mode := .normal, atLineStart := true }, hashes ++ "\n")
    else
      -- Not a header, emit hashes literally and continue
      let hashes := String.mk (List.replicate count '#')
      ({ s with mode := .normal, atLineStart := false }, hashes ++ c.toString)

  | .inHeader level =>
    if c == '\n' then
      -- End of header line - only style the content, not the hashmarks
      let output := styled s.buffer (headerStyle level)
      ({ s with mode := .normal, buffer := "", atLineStart := true }, output ++ "\n")
    else
      ({ s with buffer := s.buffer.push c, atLineStart := false }, "")

  | .inLinkText =>
    if c == ']' then
      -- End of link text, save it and look for (
      ({ s with mode := .sawLinkClose, linkText := s.buffer, buffer := "", atLineStart := newLineStart }, "")
    else if c == '\n' then
      -- Newline in link text - not a valid link, emit literally
      ({ s with mode := .normal, buffer := "", atLineStart := true }, "[" ++ s.buffer ++ "\n")
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

  | .sawLinkClose =>
    if c == '(' then
      -- Start of URL
      ({ s with mode := .inLinkUrl, buffer := "", atLineStart := newLineStart }, "")
    else
      -- Not a link, emit the bracketed text literally
      let output := "[" ++ s.linkText ++ "]" ++ c.toString
      ({ s with mode := .normal, linkText := "", atLineStart := newLineStart }, output)

  | .inLinkUrl =>
    if c == ')' then
      -- End of link - render as clickable OSC 8 hyperlink with styled text
      -- Format: \x1b]8;;URL\x07TEXT\x1b]8;;\x07 (using BEL as String Terminator)
      let styledText := styled s.linkText linkStyle
      let output := s!"\x1b]8;;{s.buffer}\x07{styledText}\x1b]8;;\x07"
      ({ s with mode := .normal, buffer := "", linkText := "", atLineStart := newLineStart }, output)
    else if c == '\n' then
      -- Newline in URL - not valid, emit literally
      ({ s with mode := .normal, buffer := "", linkText := "", atLineStart := true },
        "[" ++ s.linkText ++ "](" ++ s.buffer ++ "\n")
    else
      ({ s with buffer := s.buffer.push c, atLineStart := newLineStart }, "")

/-- Process a chunk of text, returning new state and output -/
def feed (s : State) (chunk : String) : State × String := Id.run do
  let mut state := s
  let mut output := ""
  for c in chunk.toList do
    let (newState, newOutput) := step state c
    state := newState
    output := output ++ newOutput
  (state, output)

/-- Flush any remaining buffered content at end of stream -/
def finish (s : State) : String :=
  match s.mode with
  | .normal => ""
  | .sawStar => "*"
  | .inBold => "**" ++ s.buffer  -- Unclosed bold, emit literally
  | .sawStarInBold => "**" ++ s.buffer ++ "*"
  | .inItalicStar => "*" ++ s.buffer  -- Unclosed italic
  | .sawUnder => "_"
  | .inItalicUnder => "_" ++ s.buffer
  | .inCode => "`" ++ s.buffer  -- Unclosed code
  | .sawHash count => String.mk (List.replicate count '#')
  | .inHeader level =>
    -- Emit header even without trailing newline - only style the content
    styled s.buffer (headerStyle level)
  | .inLinkText => "[" ++ s.buffer  -- Unclosed link text
  | .sawLinkClose => "[" ++ s.linkText ++ "]"  -- Link text but no URL
  | .inLinkUrl => "[" ++ s.linkText ++ "](" ++ s.buffer  -- Unclosed URL

/-- Convenience: render a complete markdown string -/
def render (input : String) : String :=
  let (state, output) := feed State.new input
  output ++ finish state

end Parlance.Markdown
