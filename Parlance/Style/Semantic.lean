/-
  Parlance.Style.Semantic - Semantic output helpers (error, warning, success, info)
-/

import Parlance.Style.Styled
import Parlance.Core.Error

namespace Parlance

open Style

/-- Style for error messages (red, bold) -/
def errorStyle : Style := { fg := .red, modifier := { bold := true } }

/-- Style for warning messages (yellow) -/
def warningStyle : Style := { fg := .yellow }

/-- Style for success messages (green) -/
def successStyle : Style := { fg := .green }

/-- Style for info messages (blue) -/
def infoStyle : Style := { fg := .blue }

/-- Style for hints/dim text -/
def hintStyle : Style := { fg := .ansi .brightBlack }

/-- Print an error message in red -/
def printError (msg : String) : IO Unit :=
  Style.println { text := s!"error: {msg}", style := errorStyle }

/-- Print a warning message in yellow -/
def printWarning (msg : String) : IO Unit :=
  Style.println { text := s!"warning: {msg}", style := warningStyle }

/-- Print a success message in green -/
def printSuccess (msg : String) : IO Unit :=
  Style.println { text := s!"success: {msg}", style := successStyle }

/-- Print an info message in blue -/
def printInfo (msg : String) : IO Unit :=
  Style.println { text := s!"info: {msg}", style := infoStyle }

/-- Print a hint in dim text -/
def printHint (msg : String) : IO Unit :=
  Style.println { text := msg, style := hintStyle }

/-- Print a parse error with formatting -/
def printParseError (e : ParseError) : IO Unit :=
  printError (ParseError.toString e)

-- Styled text helpers for building complex output
namespace Text

def error (msg : String) : StyledText := { text := msg, style := errorStyle }
def warning (msg : String) : StyledText := { text := msg, style := warningStyle }
def success (msg : String) : StyledText := { text := msg, style := successStyle }
def info (msg : String) : StyledText := { text := msg, style := infoStyle }
def hint (msg : String) : StyledText := { text := msg, style := hintStyle }
def bold (msg : String) : StyledText := { text := msg, style := Style.bold }
def dim (msg : String) : StyledText := { text := msg, style := Style.dim }
def underline (msg : String) : StyledText := { text := msg, style := Style.underline }

end Text

end Parlance
