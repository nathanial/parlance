/-
  Parlance.Parse.Tokenizer - Tokenize command-line arguments
-/

namespace Parlance.Parse

/-- A token from command-line argument parsing -/
inductive Token where
  /-- Short flag like -v or -abc (combined) -/
  | shortFlag (c : Char)
  /-- Long flag like --verbose -/
  | longFlag (name : String)
  /-- Long flag with value like --output=file -/
  | longFlagValue (name : String) (value : String)
  /-- Short flag with attached value or cluster like -ofile or -abc -/
  | shortFlagValue (c : Char) (value : String)
  /-- Positional argument or flag value -/
  | positional (value : String)
  /-- End of flags marker (--) -/
  | endOfFlags
  deriving Repr, BEq, Inhabited

namespace Token

def isFlag : Token → Bool
  | .shortFlag _ => true
  | .longFlag _ => true
  | .longFlagValue _ _ => true
  | .shortFlagValue _ _ => true
  | _ => false

def isPositional : Token → Bool
  | .positional _ => true
  | _ => false

end Token

/-- Tokenizer state -/
structure TokenizerState where
  /-- Remaining arguments to process -/
  args : List String
  /-- Whether we've seen -- (end of flags) -/
  endOfFlags : Bool := false
  deriving Inhabited

/-- Find the index of a character in a string -/
private def findCharIndex (s : String) (c : Char) : Option Nat :=
  let chars := s.toList
  chars.findIdx? (· == c)

/-- Tokenize the body of a short-flag argument (no leading '-') -/
def tokenizeShortGroup (body : String) : List Token :=
  match body.toList with
  | [] => []
  | [c] => [.shortFlag c]
  | c :: rest => [.shortFlagValue c (String.ofList rest)]

/-- Tokenize a single argument string -/
def tokenizeOne (arg : String) (afterEndOfFlags : Bool) : List Token :=
  if afterEndOfFlags then
    [.positional arg]
  else if arg == "--" then
    [.endOfFlags]
  else if arg.startsWith "--" then
    let rest := arg.drop 2
    match findCharIndex rest '=' with
    | some idx =>
      let name := (rest.toList.take idx).asString
      let value := (rest.toList.drop (idx + 1)).asString
      [.longFlagValue name value]
    | none =>
      [.longFlag rest]
  else if arg.startsWith "-" && arg.length > 1 then
    tokenizeShortGroup (arg.drop 1)
  else
    [.positional arg]

/-- Tokenize a list of argument strings -/
def tokenize (args : List String) : List Token :=
  go args false []
where
  go : List String → Bool → List Token → List Token
  | [], _, acc => acc.reverse
  | arg :: rest, afterEnd, acc =>
    let tokens := tokenizeOne arg afterEnd
    let newAfterEnd := afterEnd || tokens.any (· == .endOfFlags)
    go rest newAfterEnd (tokens.reverse ++ acc)

/-- Get the flag name from a token (if it's a flag) -/
def Token.flagName? : Token → Option String
  | .shortFlag c => some (String.singleton c)
  | .longFlag name => some name
  | .longFlagValue name _ => some name
  | .shortFlagValue c _ => some (String.singleton c)
  | _ => none

/-- Get the value from a flag token (if it has one) -/
def Token.flagValue? : Token → Option String
  | .longFlagValue _ value => some value
  | .shortFlagValue _ value => some value
  | _ => none

end Parlance.Parse
