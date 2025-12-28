/-
  Parlance Test Suite
-/

import Crucible
import Parlance
import Staple

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

-- Completion Tests
namespace Tests.Completion

open Parlance.Completion
open Staple (String.containsSubstr)

testSuite "Completion"

-- Helper test command
def testCmd : Command := command "myapp" do
  Cmd.version "1.0.0"
  Cmd.description "A test application"
  Cmd.boolFlag "verbose" (short := some 'v') (description := "Enable verbose output")
  Cmd.flag "output" (short := some 'o') (argType := .path) (description := "Output file")
  Cmd.flag "format" (argType := .choice ["json", "yaml", "toml"]) (description := "Output format")
  Cmd.subcommand "init" do
    Cmd.description "Initialize a new project"
    Cmd.boolFlag "force" (description := "Force initialization")
  Cmd.subcommand "build" do
    Cmd.description "Build the project"
    Cmd.flag "target" (argType := .string) (description := "Build target")

test "Shell.fromString parses bash" :=
  Shell.fromString "bash" ≡ some .bash

test "Shell.fromString parses zsh" :=
  Shell.fromString "zsh" ≡ some .zsh

test "Shell.fromString parses fish" :=
  Shell.fromString "fish" ≡ some .fish

test "Shell.fromString is case insensitive" :=
  Shell.fromString "BASH" ≡ some .bash

test "Shell.fromString returns none for invalid" := do
  shouldBeNone (Shell.fromString "invalid")
  shouldBeNone (Shell.fromString "")

test "Shell.toString roundtrips" := do
  Shell.bash.toString ≡ "bash"
  Shell.zsh.toString ≡ "zsh"
  Shell.fish.toString ≡ "fish"

test "flagCompletions includes long forms" := do
  let flags := flagCompletions testCmd
  shouldSatisfy (flags.contains "--verbose") "should contain --verbose"
  shouldSatisfy (flags.contains "--output") "should contain --output"
  shouldSatisfy (flags.contains "--format") "should contain --format"

test "flagCompletions includes short forms" := do
  let flags := flagCompletions testCmd
  shouldSatisfy (flags.contains "-v") "should contain -v"
  shouldSatisfy (flags.contains "-o") "should contain -o"

test "subcommandCompletions lists subcommands" := do
  let subcmds := subcommandCompletions testCmd
  subcmds ≡ ["init", "build"]

test "escapeForShell escapes single quotes" :=
  escapeForShell "it's" ≡ "it'\\''s"

test "escapeForShell handles no quotes" :=
  escapeForShell "hello world" ≡ "hello world"

test "Bash script contains function definition" := do
  let script := Bash.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "_myapp_completions()") "should define completion function"

test "Bash script contains complete command" := do
  let script := Bash.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "complete -F _myapp_completions myapp") "should register completion"

test "Bash script includes flags" := do
  let script := Bash.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "--verbose") "should include verbose flag"
  shouldSatisfy (script.containsSubstr "--output") "should include output flag"

test "Bash script includes choice options" := do
  let script := Bash.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "json yaml toml") "should include choice options"

test "Bash script includes subcommands" := do
  let script := Bash.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "init") "should include init subcommand"
  shouldSatisfy (script.containsSubstr "build") "should include build subcommand"

test "Zsh script contains compdef" := do
  let script := Zsh.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "#compdef myapp") "should have compdef header"

test "Zsh script contains function definition" := do
  let script := Zsh.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "_myapp()") "should define completion function"

test "Zsh script includes _files for path type" := do
  let script := Zsh.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "_files") "should use _files for path completion"

test "Fish script uses complete command" := do
  let script := Fish.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "complete -c myapp") "should use complete -c command"

test "Fish script includes long flag" := do
  let script := Fish.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "-l verbose") "should include long flag"

test "Fish script includes short flag" := do
  let script := Fish.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "-s v") "should include short flag"

test "Fish script includes subcommands" := do
  let script := Fish.generateScript testCmd "myapp"
  shouldSatisfy (script.containsSubstr "-a 'init'") "should complete init subcommand"
  shouldSatisfy (script.containsSubstr "-a 'build'") "should complete build subcommand"

test "generateScript dispatches to bash" := do
  let script := generateScript testCmd "myapp" .bash
  shouldSatisfy (script.containsSubstr "complete -F") "bash uses complete -F"

test "generateScript dispatches to zsh" := do
  let script := generateScript testCmd "myapp" .zsh
  shouldSatisfy (script.containsSubstr "#compdef") "zsh uses #compdef"

test "generateScript dispatches to fish" := do
  let script := generateScript testCmd "myapp" .fish
  shouldSatisfy (script.containsSubstr "complete -c") "fish uses complete -c"

test "handleCompletionRequest returns some for valid request" := do
  let result := handleCompletionRequest testCmd "myapp" ["--generate-completion", "bash"]
  shouldSatisfy result.isSome "should return some for valid request"

test "handleCompletionRequest returns none for missing flag" := do
  let result := handleCompletionRequest testCmd "myapp" ["--verbose", "file.txt"]
  shouldSatisfy result.isNone "should return none when flag not present"

test "handleCompletionRequest returns none for missing shell arg" := do
  let result := handleCompletionRequest testCmd "myapp" ["--generate-completion"]
  shouldSatisfy result.isNone "should return none when shell arg missing"

test "handleCompletionRequest returns none for invalid shell" := do
  let result := handleCompletionRequest testCmd "myapp" ["--generate-completion", "invalid"]
  shouldSatisfy result.isNone "should return none for invalid shell"

#generate_tests

end Tests.Completion

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
