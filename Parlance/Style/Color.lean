/-
  Parlance.Style.Color - Color type definitions
-/

import Parlance.Style.Ansi

namespace Parlance.Style

/-- Standard 16 ANSI colors -/
inductive Color16 where
  | black
  | red
  | green
  | yellow
  | blue
  | magenta
  | cyan
  | white
  | brightBlack
  | brightRed
  | brightGreen
  | brightYellow
  | brightBlue
  | brightMagenta
  | brightCyan
  | brightWhite
  deriving Repr, BEq, Inhabited

namespace Color16

/-- Get the ANSI foreground code for this color -/
def toFgCode : Color16 → String
  | .black => Ansi.fgBlack
  | .red => Ansi.fgRed
  | .green => Ansi.fgGreen
  | .yellow => Ansi.fgYellow
  | .blue => Ansi.fgBlue
  | .magenta => Ansi.fgMagenta
  | .cyan => Ansi.fgCyan
  | .white => Ansi.fgWhite
  | .brightBlack => Ansi.fgBrightBlack
  | .brightRed => Ansi.fgBrightRed
  | .brightGreen => Ansi.fgBrightGreen
  | .brightYellow => Ansi.fgBrightYellow
  | .brightBlue => Ansi.fgBrightBlue
  | .brightMagenta => Ansi.fgBrightMagenta
  | .brightCyan => Ansi.fgBrightCyan
  | .brightWhite => Ansi.fgBrightWhite

/-- Get the ANSI background code for this color -/
def toBgCode : Color16 → String
  | .black => Ansi.bgBlack
  | .red => Ansi.bgRed
  | .green => Ansi.bgGreen
  | .yellow => Ansi.bgYellow
  | .blue => Ansi.bgBlue
  | .magenta => Ansi.bgMagenta
  | .cyan => Ansi.bgCyan
  | .white => Ansi.bgWhite
  | .brightBlack => Ansi.bgBrightBlack
  | .brightRed => Ansi.bgBrightRed
  | .brightGreen => Ansi.bgBrightGreen
  | .brightYellow => Ansi.bgBrightYellow
  | .brightBlue => Ansi.bgBrightBlue
  | .brightMagenta => Ansi.bgBrightMagenta
  | .brightCyan => Ansi.bgBrightCyan
  | .brightWhite => Ansi.bgBrightWhite

end Color16

/-- Color specification supporting multiple color modes -/
inductive Color where
  | default
  | ansi (c : Color16)
  | indexed (n : UInt8)
  | rgb (r g b : UInt8)
  deriving Repr, BEq, Inhabited

namespace Color

-- Convenience constructors for standard colors
def black : Color := .ansi .black
def red : Color := .ansi .red
def green : Color := .ansi .green
def yellow : Color := .ansi .yellow
def blue : Color := .ansi .blue
def magenta : Color := .ansi .magenta
def cyan : Color := .ansi .cyan
def white : Color := .ansi .white

def brightBlack : Color := .ansi .brightBlack
def brightRed : Color := .ansi .brightRed
def brightGreen : Color := .ansi .brightGreen
def brightYellow : Color := .ansi .brightYellow
def brightBlue : Color := .ansi .brightBlue
def brightMagenta : Color := .ansi .brightMagenta
def brightCyan : Color := .ansi .brightCyan
def brightWhite : Color := .ansi .brightWhite

def gray : Color := .ansi .brightBlack

/-- Get the ANSI foreground code for this color -/
def toFgCode : Color → Option String
  | .default => none
  | .ansi c => some c.toFgCode
  | .indexed n => some (Ansi.fg256 n)
  | .rgb r g b => some (Ansi.fgRgb r g b)

/-- Get the ANSI background code for this color -/
def toBgCode : Color → Option String
  | .default => none
  | .ansi c => some c.toBgCode
  | .indexed n => some (Ansi.bg256 n)
  | .rgb r g b => some (Ansi.bgRgb r g b)

end Color

end Parlance.Style
