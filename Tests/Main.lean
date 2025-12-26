/-
  Parlance Test Suite
-/

import Crucible
import Parlance

open Crucible
open Parlance

-- Core Types Tests
namespace Tests.CoreTypes

testSuite "CoreTypes"

test "ArgType toString" :=
  ArgType.string.toString ≡ "STRING"

test "Flag isBoolean" := do
  let boolFlag : Flag := { long := "verbose" }
  let valueFlag : Flag := { long := "output", argType := some .string }
  shouldSatisfy boolFlag.isBoolean "bool flag should be boolean"
  shouldSatisfy (!valueFlag.isBoolean) "value flag should not be boolean"

test "ParsedValues setValue and getValue" := do
  let pv := ParsedValues.empty.setValue "name" "value"
  pv.getValue "name" ≡ some "value"

test "ParsedValues setBool and hasBool" := do
  let pv := ParsedValues.empty.setBool "verbose"
  shouldSatisfy (pv.hasBool "verbose") "should have verbose"
  shouldSatisfy (!pv.hasBool "debug") "should not have debug"

#generate_tests

end Tests.CoreTypes

-- Tokenizer Tests
namespace Tests.Tokenizer

open Parlance.Parse

testSuite "Tokenizer"

test "tokenize long flag" :=
  tokenize ["--verbose"] ≡ [.longFlag "verbose"]

test "tokenize long flag with value" :=
  tokenize ["--output=file.txt"] ≡ [.longFlagValue "output" "file.txt"]

test "tokenize short flags" :=
  tokenize ["-v"] ≡ [.shortFlag 'v']

test "tokenize positional" :=
  tokenize ["file.txt"] ≡ [.positional "file.txt"]

test "tokenize end of flags" :=
  tokenize ["--", "--not-a-flag"] ≡ [.endOfFlags, .positional "--not-a-flag"]

#generate_tests

end Tests.Tokenizer

-- Parser Tests
namespace Tests.Parser

testSuite "Parser"

test "parse simple positional arg" := do
  let cmd := command "test" do
    Cmd.arg "file"
  match parse cmd ["input.txt"] with
  | .ok result =>
    let val := result.getString "file"
    if val != some "input.txt" then
      throw (IO.userError s!"Expected some \"input.txt\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parse boolean flag" := do
  let cmd := command "test" do
    Cmd.boolFlag "verbose" (short := some 'v')
  match parse cmd ["--verbose"] with
  | .ok result => shouldSatisfy (result.hasFlag "verbose") "should have verbose flag"
  | .error _ => throw (IO.userError "Expected success")

test "parse help flag" := do
  let cmd := command "test" do
    Cmd.boolFlag "verbose"
  match parse cmd ["--help"] with
  | .ok _ => throw (IO.userError "Expected help error")
  | .error e => e ≡ .helpRequested

test "parse unknown flag error" := do
  let cmd := command "test" (pure ())
  match parse cmd ["--unknown"] with
  | .ok _ => throw (IO.userError "Expected unknown flag error")
  | .error e =>
    match e with
    | .unknownFlag "unknown" => pure ()
    | _ => throw (IO.userError s!"Expected unknownFlag, got {e}")

#generate_tests

end Tests.Parser

-- Extractor Tests
namespace Tests.Extractor

testSuite "Extractor"

test "FromArg String" :=
  FromArg.parse (α := String) "hello" ≡ some "hello"

test "FromArg Nat" :=
  FromArg.parse (α := Nat) "42" ≡ some 42

test "FromArg Bool true" :=
  FromArg.parse (α := Bool) "true" ≡ some true

test "FromArg Bool false" :=
  FromArg.parse (α := Bool) "false" ≡ some false

#generate_tests

end Tests.Extractor

-- Style Tests
namespace Tests.Style

open Parlance.Style

testSuite "Style"

test "Color16 toFgCode" :=
  Color16.red.toFgCode ≡ "31"

test "Style isEmpty default" :=
  Style.default.isEmpty ≡ true

test "Style isEmpty bold" :=
  Style.bold.isEmpty ≡ false

#generate_tests

end Tests.Style

-- Command Builder Tests
namespace Tests.CommandBuilder

testSuite "CommandBuilder"

test "build simple command" := do
  let cmd := command "myapp" do
    Cmd.version "1.0.0"
    Cmd.description "My application"
  cmd.name ≡ "myapp"
  cmd.version ≡ "1.0.0"

test "build command with flags" := do
  let cmd := command "myapp" do
    Cmd.boolFlag "verbose" (short := some 'v')
    Cmd.flag "output" (argType := .path)
  cmd.flags.size ≡ 2

#generate_tests

end Tests.CommandBuilder

-- Help Generation Tests
namespace Tests.HelpGeneration

testSuite "HelpGeneration"

test "Command.usage simple" := do
  let cmd := command "myapp" (pure ())
  cmd.usage ≡ "myapp"

test "Command.usage with flags" := do
  let cmd := command "myapp" do
    Cmd.boolFlag "verbose"
  cmd.usage ≡ "myapp [OPTIONS]"

#generate_tests

end Tests.HelpGeneration

-- Table Tests
namespace Tests.Tables

testSuite "Tables"

test "Table.simple creation" := do
  let t := Output.Table.simple ["A", "B"] [["1", "2"], ["3", "4"]]
  t.columns.size ≡ 2
  t.rows.size ≡ 2

#generate_tests

end Tests.Tables

-- Main test runner
def main : IO UInt32 := do
  IO.println "Parlance CLI Library Tests"
  IO.println "=========================="
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result != 0 then
    IO.println "Some tests failed!"
    return 1
  else
    IO.println "All tests passed!"
    return 0
