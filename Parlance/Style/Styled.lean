/-
  Parlance.Style.Styled - Styled text rendering
-/

import Parlance.Style.Ansi
import Parlance.Style.Color
import Parlance.Style.Modifier

namespace Parlance.Style

/-- Complete style specification -/
structure Style where
  fg : Color := .default
  bg : Color := .default
  modifier : Modifier := {}
  deriving Repr, BEq, Inhabited

namespace Style

def default : Style := {}

-- Style constructors
def fgColor (c : Color) : Style := { fg := c }
def bgColor (c : Color) : Style := { bg := c }

def bold : Style := { modifier := Modifier.mkBold }
def dim : Style := { modifier := Modifier.mkDim }
def italic : Style := { modifier := Modifier.mkItalic }
def underline : Style := { modifier := Modifier.mkUnderline }
def blink : Style := { modifier := Modifier.mkBlink }
def reversed : Style := { modifier := Modifier.mkReverse }
def hidden : Style := { modifier := Modifier.mkHidden }
def strikethrough : Style := { modifier := Modifier.mkStrikethrough }

-- Fluent modifiers
def withFg (s : Style) (c : Color) : Style := { s with fg := c }
def withBg (s : Style) (c : Color) : Style := { s with bg := c }
def withModifier (s : Style) (m : Modifier) : Style :=
  { s with modifier := Modifier.merge s.modifier m }
def withBold (s : Style) : Style := s.withModifier Modifier.mkBold
def withDim (s : Style) : Style := s.withModifier Modifier.mkDim
def withItalic (s : Style) : Style := s.withModifier Modifier.mkItalic
def withUnderline (s : Style) : Style := s.withModifier Modifier.mkUnderline

/-- Merge two styles -/
def merge (s1 s2 : Style) : Style := {
  fg := if s2.fg == .default then s1.fg else s2.fg
  bg := if s2.bg == .default then s1.bg else s2.bg
  modifier := Modifier.merge s1.modifier s2.modifier
}

/-- Check if style is the default (no styling) -/
def isEmpty (s : Style) : Bool :=
  s.fg == .default && s.bg == .default && s.modifier.isEmpty

/-- Convert style to list of ANSI codes -/
def toCodes (s : Style) : List String :=
  let modCodes := s.modifier.toCodes
  let fgCode := s.fg.toFgCode.toList
  let bgCode := s.bg.toBgCode.toList
  modCodes ++ fgCode ++ bgCode

instance : Append Style where
  append := merge

end Style

/-- Text with associated style -/
structure StyledText where
  text : String
  style : Style := {}
  deriving Repr, Inhabited

namespace StyledText

/-- Render styled text to ANSI string -/
def render (st : StyledText) : String :=
  if st.style.isEmpty then st.text
  else
    let codes := st.style.toCodes
    if codes.isEmpty then st.text
    else s!"{Ansi.sgr codes}{st.text}{Ansi.reset}"

/-- Create styled text with a style -/
def styled (text : String) (style : Style) : StyledText :=
  { text, style }

/-- Create plain text (no styling) -/
def plain (text : String) : StyledText :=
  { text }

end StyledText

/-- Print styled text to stdout -/
def print (st : StyledText) : IO Unit :=
  IO.print st.render

/-- Print styled text with newline -/
def println (st : StyledText) : IO Unit :=
  IO.println st.render

/-- Style a string and print it -/
def printStyled (text : String) (style : Style) : IO Unit :=
  println { text, style }

end Parlance.Style
