/-
  Parlance.Command.Builder - Monadic command builder (following Scribe's HtmlM pattern)
-/

import Parlance.Core.Types

namespace Parlance

/-- Builder state for constructing commands -/
structure CommandState where
  name : String := ""
  version : String := ""
  description : String := ""
  flags : Array Flag := #[]
  args : Array Arg := #[]
  subcommands : Array Command := #[]
  deriving Inhabited

/-- Command builder monad -/
def CommandM := StateM CommandState

instance : Monad CommandM where
  pure a := (pure a : StateM CommandState _)
  bind ma f := (ma >>= f : StateM CommandState _)

instance : MonadState CommandState CommandM where
  get := (get : StateM CommandState _)
  set s := (set s : StateM CommandState _)
  modifyGet f := (modifyGet f : StateM CommandState _)

namespace CommandM

/-- Set the version string -/
def version (v : String) : CommandM Unit :=
  modify fun s => { s with version := v }

/-- Set the description -/
def description (d : String) : CommandM Unit :=
  modify fun s => { s with description := d }

/-- Add a boolean flag (no value required) -/
def boolFlag (long : String) (short : Option Char := none)
    (description : String := "")
    (envVar : Option String := none)
    (negatable : Bool := false) : CommandM Unit :=
  modify fun s => { s with
    flags := s.flags.push {
      long := long
      short := short
      argType := none
      description := description
      required := false
      envVar := envVar
      negatable := negatable
    }
  }

/-- Add a flag with a value -/
def flag (long : String) (short : Option Char := none)
    (argType : ArgType := .string)
    (description : String := "")
    (defaultValue : Option String := none)
    (required : Bool := false)
    (envVar : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  modify fun s => { s with
    flags := s.flags.push {
      long := long
      short := short
      argType := some argType
      description := description
      defaultValue := defaultValue
      required := required
      envVar := envVar
      validate := validate
    }
  }

/-- Add a repeatable flag (can be specified multiple times to collect values) -/
def repeatableFlag (long : String) (short : Option Char := none)
    (argType : ArgType := .string)
    (description : String := "")
    (defaultValue : Option String := none)
    (required : Bool := false)
    (envVar : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  modify fun s => { s with
    flags := s.flags.push {
      long := long
      short := short
      argType := some argType
      description := description
      defaultValue := defaultValue
      required := required
      envVar := envVar
      repeatable := true
      validate := validate
    }
  }

/-- Add a positional argument -/
def arg (name : String) (argType : ArgType := .string)
    (description : String := "")
    (required : Bool := true)
    (defaultValue : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  modify fun s => { s with
    args := s.args.push {
      name := name
      argType := argType
      description := description
      required := required
      defaultValue := defaultValue
      validate := validate
    }
  }

/-- Convert state to Command -/
private def toCommand (s : CommandState) : Command := {
  name := s.name
  version := s.version
  description := s.description
  flags := s.flags
  args := s.args
  subcommands := s.subcommands
}

/-- Build a command from a builder -/
def build (name : String) (m : CommandM Unit) : Command :=
  let initialState : CommandState := { name := name }
  let (_, state) := (m : StateM CommandState Unit).run initialState
  toCommand state

/-- Add a subcommand -/
def subcommand (name : String) (builder : CommandM Unit) : CommandM Unit := do
  let subcmd := build name builder
  modify fun s => { s with subcommands := s.subcommands.push subcmd }

end CommandM

/-- Top-level command builder -/
def command (name : String) (builder : CommandM Unit) : Command :=
  CommandM.build name builder

-- Re-export CommandM methods at top level for nicer syntax
namespace Cmd

def version (v : String) : CommandM Unit := CommandM.version v

def description (d : String) : CommandM Unit := CommandM.description d

def boolFlag (long : String) (short : Option Char := none)
    (description : String := "")
    (envVar : Option String := none)
    (negatable : Bool := false) : CommandM Unit :=
  CommandM.boolFlag long short description envVar negatable

def flag (long : String) (short : Option Char := none)
    (argType : ArgType := .string)
    (description : String := "")
    (defaultValue : Option String := none)
    (required : Bool := false)
    (envVar : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  CommandM.flag long short argType description defaultValue required envVar validate

def repeatableFlag (long : String) (short : Option Char := none)
    (argType : ArgType := .string)
    (description : String := "")
    (defaultValue : Option String := none)
    (required : Bool := false)
    (envVar : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  CommandM.repeatableFlag long short argType description defaultValue required envVar validate

def arg (name : String) (argType : ArgType := .string)
    (description : String := "")
    (required : Bool := true)
    (defaultValue : Option String := none)
    (validate : Option (String → Except String Unit) := none) : CommandM Unit :=
  CommandM.arg name argType description required defaultValue validate

def subcommand (name : String) (builder : CommandM Unit) : CommandM Unit :=
  CommandM.subcommand name builder

end Cmd

end Parlance
