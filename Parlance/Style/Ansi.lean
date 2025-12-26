/-
  Parlance.Style.Ansi - ANSI escape code primitives
-/

namespace Parlance.Style.Ansi

/-- ANSI escape sequence prefix -/
def esc : String := "\x1b["

/-- Reset all attributes -/
def reset : String := esc ++ "0m"

/-- Apply SGR (Select Graphic Rendition) codes -/
def sgr (codes : List String) : String :=
  if codes.isEmpty then ""
  else esc ++ ";".intercalate codes ++ "m"

-- Text attribute codes
def codeBold : String := "1"
def codeDim : String := "2"
def codeItalic : String := "3"
def codeUnderline : String := "4"
def codeBlink : String := "5"
def codeReverse : String := "7"
def codeHidden : String := "8"
def codeStrikethrough : String := "9"

-- Standard foreground colors (30-37)
def fgBlack : String := "30"
def fgRed : String := "31"
def fgGreen : String := "32"
def fgYellow : String := "33"
def fgBlue : String := "34"
def fgMagenta : String := "35"
def fgCyan : String := "36"
def fgWhite : String := "37"

-- Bright foreground colors (90-97)
def fgBrightBlack : String := "90"
def fgBrightRed : String := "91"
def fgBrightGreen : String := "92"
def fgBrightYellow : String := "93"
def fgBrightBlue : String := "94"
def fgBrightMagenta : String := "95"
def fgBrightCyan : String := "96"
def fgBrightWhite : String := "97"

-- Standard background colors (40-47)
def bgBlack : String := "40"
def bgRed : String := "41"
def bgGreen : String := "42"
def bgYellow : String := "43"
def bgBlue : String := "44"
def bgMagenta : String := "45"
def bgCyan : String := "46"
def bgWhite : String := "47"

-- Bright background colors (100-107)
def bgBrightBlack : String := "100"
def bgBrightRed : String := "101"
def bgBrightGreen : String := "102"
def bgBrightYellow : String := "103"
def bgBrightBlue : String := "104"
def bgBrightMagenta : String := "105"
def bgBrightCyan : String := "106"
def bgBrightWhite : String := "107"

/-- 256-color foreground: ESC[38;5;Nm -/
def fg256 (n : UInt8) : String := s!"38;5;{n}"

/-- 256-color background: ESC[48;5;Nm -/
def bg256 (n : UInt8) : String := s!"48;5;{n}"

/-- RGB foreground: ESC[38;2;R;G;Bm -/
def fgRgb (r g b : UInt8) : String := s!"38;2;{r};{g};{b}"

/-- RGB background: ESC[48;2;R;G;Bm -/
def bgRgb (r g b : UInt8) : String := s!"48;2;{r};{g};{b}"

-- Cursor control
def cursorUp (n : Nat := 1) : String := s!"{esc}{n}A"
def cursorDown (n : Nat := 1) : String := s!"{esc}{n}B"
def cursorForward (n : Nat := 1) : String := s!"{esc}{n}C"
def cursorBack (n : Nat := 1) : String := s!"{esc}{n}D"

/-- Move cursor to beginning of line -/
def cursorColumn (n : Nat := 1) : String := s!"{esc}{n}G"

/-- Carriage return (move to start of line) -/
def cr : String := "\r"

/-- Clear from cursor to end of line -/
def clearToEndOfLine : String := esc ++ "K"

/-- Clear entire line -/
def clearLine : String := esc ++ "2K"

/-- Hide cursor -/
def hideCursor : String := esc ++ "?25l"

/-- Show cursor -/
def showCursor : String := esc ++ "?25h"

end Parlance.Style.Ansi
