/-
  Parlance.Validate - Typed validator constructors for declarative validation
-/

namespace Parlance.Validate

/-- Validate a Nat value with a predicate -/
def nat (pred : Nat → Bool) (msg : String) : String → Except String Unit :=
  fun s => match s.toNat? with
    | some n => if pred n then .ok () else .error msg
    | none => .ok ()  -- Type parsing error handled elsewhere

/-- Validate an Int value with a predicate -/
def int (pred : Int → Bool) (msg : String) : String → Except String Unit :=
  fun s => match s.toInt? with
    | some n => if pred n then .ok () else .error msg
    | none => .ok ()

/-- Validate a string value with a predicate -/
def string (pred : String → Bool) (msg : String) : String → Except String Unit :=
  fun s => if pred s then .ok () else .error msg

/-- Port number validator (0-65535) -/
def port : String → Except String Unit :=
  nat (· < 65536) "Port must be between 0 and 65535"

/-- Validate that a Nat is in a range [min, max) -/
def natRange (min max : Nat) (msg : Option String := none) : String → Except String Unit :=
  nat (fun n => min ≤ n && n < max)
    (msg.getD s!"Must be between {min} and {max}")

/-- Validate that a Nat is less than a maximum -/
def natLt (max : Nat) (msg : Option String := none) : String → Except String Unit :=
  nat (· < max) (msg.getD s!"Must be less than {max}")

/-- Validate that a Nat is at most a maximum -/
def natLe (max : Nat) (msg : Option String := none) : String → Except String Unit :=
  nat (· ≤ max) (msg.getD s!"Must be at most {max}")

/-- Validate that a Nat is greater than a minimum -/
def natGt (min : Nat) (msg : Option String := none) : String → Except String Unit :=
  nat (· > min) (msg.getD s!"Must be greater than {min}")

/-- Validate that a Nat is at least a minimum -/
def natGe (min : Nat) (msg : Option String := none) : String → Except String Unit :=
  nat (· ≥ min) (msg.getD s!"Must be at least {min}")

/-- Validate that an Int is positive -/
def intPositive (msg : Option String := none) : String → Except String Unit :=
  int (· > 0) (msg.getD "Must be positive")

/-- Validate that an Int is non-negative -/
def intNonNegative (msg : Option String := none) : String → Except String Unit :=
  int (· ≥ 0) (msg.getD "Must be non-negative")

/-- Validate that an Int is in a range [min, max] -/
def intRange (min max : Int) (msg : Option String := none) : String → Except String Unit :=
  int (fun n => min ≤ n && n ≤ max)
    (msg.getD s!"Must be between {min} and {max}")

/-- Validate string is non-empty -/
def nonEmpty (msg : Option String := none) : String → Except String Unit :=
  string (!·.isEmpty) (msg.getD "Cannot be empty")

/-- Validate string has minimum length -/
def minLength (n : Nat) (msg : Option String := none) : String → Except String Unit :=
  string (·.length ≥ n) (msg.getD s!"Must be at least {n} characters")

/-- Validate string has maximum length -/
def maxLength (n : Nat) (msg : Option String := none) : String → Except String Unit :=
  string (·.length ≤ n) (msg.getD s!"Must be at most {n} characters")

/-- Validate string length is in a range [min, max] -/
def lengthRange (min max : Nat) (msg : Option String := none) : String → Except String Unit :=
  string (fun s => s.length ≥ min && s.length ≤ max)
    (msg.getD s!"Length must be between {min} and {max} characters")

/-- Combine two validators (both must pass) -/
def and (v1 v2 : String → Except String Unit) : String → Except String Unit :=
  fun s => do
    v1 s
    v2 s

/-- Combine multiple validators (all must pass) -/
def all (validators : List (String → Except String Unit)) : String → Except String Unit :=
  fun s => validators.forM (· s)

end Parlance.Validate
