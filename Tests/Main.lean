/-
  Parlance Test Suite
-/

import Crucible
import Parlance
import Parlance.Repl
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

test "tokenize short flag with attached value" :=
  tokenize ["-ofile"] ≡ [.shortFlagValue 'o' "file"]

test "tokenize short flag group" :=
  tokenize ["-abc"] ≡ [.shortFlagValue 'a' "bc"]

test "tokenize positional" :=
  tokenize ["file.txt"] ≡ [.positional "file.txt"]

test "tokenize end of flags" :=
  tokenize ["--", "--not-a-flag"] ≡ [.endOfFlags, .positional "--not-a-flag"]


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

test "parse negatable boolean flag" := do
  let cmd := command "test" do
    Cmd.boolFlag "color" (negatable := true)
  match parse cmd ["--no-color"] with
  | .ok result =>
    if result.hasFlag "color" then
      throw (IO.userError "Expected color flag to be unset")
    if result.getBool "color" != false then
      throw (IO.userError "Expected color flag to be false")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parse negation errors when not enabled" := do
  let cmd := command "test" do
    Cmd.boolFlag "color"
  match parse cmd ["--no-color"] with
  | .ok _ => throw (IO.userError "Expected unknown flag error")
  | .error e =>
    match e with
    | .unknownFlag "no-color" => pure ()
    | _ => throw (IO.userError s!"Expected unknownFlag, got {e}")

test "parse choice flag valid" := do
  let cmd := command "test" do
    Cmd.flag "format" (argType := .choice ["json", "yaml"])
  match parse cmd ["--format", "json"] with
  | .ok result => result.getString "format" ≡ some "json"
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parse choice flag invalid" := do
  let cmd := command "test" do
    Cmd.flag "format" (argType := .choice ["json", "yaml"])
  match parse cmd ["--format", "toml"] with
  | .ok _ => throw (IO.userError "Expected invalid choice error")
  | .error e =>
    match e with
    | .invalidChoice "format" "toml" ["json", "yaml"] => pure ()
    | _ => throw (IO.userError s!"Expected invalidChoice, got {e}")

test "parse choice positional invalid" := do
  let cmd := command "test" do
    Cmd.arg "mode" (argType := .choice ["fast", "slow"])
  match parse cmd ["turbo"] with
  | .ok _ => throw (IO.userError "Expected invalid choice error")
  | .error e =>
    match e with
    | .invalidChoice "mode" "turbo" ["fast", "slow"] => pure ()
    | _ => throw (IO.userError s!"Expected invalidChoice, got {e}")

test "parse short flag with attached value" := do
  let cmd := command "test" do
    Cmd.flag "output" (short := some 'o')
  match parse cmd ["-ofile"] with
  | .ok result =>
    let val := result.getString "output"
    if val != some "file" then
      throw (IO.userError s!"Expected some \"file\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parse short flag cluster with value tail" := do
  let cmd := command "test" do
    Cmd.boolFlag "all" (short := some 'a')
    Cmd.flag "output" (short := some 'o')
  match parse cmd ["-aofile"] with
  | .ok result =>
    if !result.hasFlag "all" then
      throw (IO.userError "Expected -a to be set")
    let val := result.getString "output"
    if val != some "file" then
      throw (IO.userError s!"Expected some \"file\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

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

test "FromArg Float decimal" :=
  FromArg.parse (α := Float) "3.14" ≡ some 3.14

test "FromArg Float negative" :=
  FromArg.parse (α := Float) "-3.14" ≡ some (-3.14)

test "FromArg Float scientific" :=
  FromArg.parse (α := Float) "1e-5" ≡ some 0.00001

test "FromArg Float integer" :=
  FromArg.parse (α := Float) "42" ≡ some 42.0

test "FromArg Float negative integer" :=
  FromArg.parse (α := Float) "-42" ≡ some (-42.0)


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


end Tests.CommandBuilder

-- Help Generation Tests
namespace Tests.HelpGeneration

open Staple (String.containsSubstr)

testSuite "HelpGeneration"

test "Command.usage simple" := do
  let cmd := command "myapp" (pure ())
  cmd.usage ≡ "myapp"

test "Command.usage with flags" := do
  let cmd := command "myapp" do
    Cmd.boolFlag "verbose"
  cmd.usage ≡ "myapp [OPTIONS]"

test "help text shows negatable flag" := do
  let cmd := command "myapp" do
    Cmd.boolFlag "color" (negatable := true)
  let help := cmd.helpText
  shouldSatisfy (help.containsSubstr "--[no-]color") "help should include negatable flag"


end Tests.HelpGeneration

-- Environment Variable Tests
namespace Tests.EnvVar

open Staple (String.containsSubstr)

testSuite "EnvVar"

test "Flag stores envVar field" := do
  let cmd := command "test" do
    Cmd.flag "token" (envVar := some "API_TOKEN")
  match cmd.flags[0]? with
  | some flag => flag.envVar ≡ some "API_TOKEN"
  | none => throw (IO.userError "Expected flag")

test "parseWithEnv uses env var value" := do
  let cmd := command "test" do
    Cmd.flag "token" (envVar := some "API_TOKEN")
  let getEnv := fun name => if name == "API_TOKEN" then some "secret123" else none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    let val := result.getString "token"
    if val != some "secret123" then
      throw (IO.userError s!"Expected some \"secret123\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv prefers command-line over env var" := do
  let cmd := command "test" do
    Cmd.flag "token" (envVar := some "API_TOKEN")
  let getEnv := fun name => if name == "API_TOKEN" then some "fromenv" else none
  match parseWithEnv cmd ["--token=fromcli"] getEnv with
  | .ok result =>
    let val := result.getString "token"
    if val != some "fromcli" then
      throw (IO.userError s!"Expected some \"fromcli\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv env var overrides default" := do
  let cmd := command "test" do
    Cmd.flag "token" (defaultValue := some "default") (envVar := some "API_TOKEN")
  let getEnv := fun name => if name == "API_TOKEN" then some "fromenv" else none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    let val := result.getString "token"
    if val != some "fromenv" then
      throw (IO.userError s!"Expected some \"fromenv\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv falls back to default when env not set" := do
  let cmd := command "test" do
    Cmd.flag "token" (defaultValue := some "default") (envVar := some "API_TOKEN")
  let getEnv := fun _ => none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    let val := result.getString "token"
    if val != some "default" then
      throw (IO.userError s!"Expected some \"default\", got {repr val}")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv boolean flag from env true" := do
  let cmd := command "test" do
    Cmd.boolFlag "verbose" (envVar := some "VERBOSE")
  let getEnv := fun name => if name == "VERBOSE" then some "true" else none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    if !result.hasFlag "verbose" then
      throw (IO.userError "Expected verbose flag to be set")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv boolean flag from env 1" := do
  let cmd := command "test" do
    Cmd.boolFlag "verbose" (envVar := some "VERBOSE")
  let getEnv := fun name => if name == "VERBOSE" then some "1" else none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    if !result.hasFlag "verbose" then
      throw (IO.userError "Expected verbose flag to be set")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "parseWithEnv boolean flag not set when env is false" := do
  let cmd := command "test" do
    Cmd.boolFlag "verbose" (envVar := some "VERBOSE")
  let getEnv := fun name => if name == "VERBOSE" then some "false" else none
  match parseWithEnv cmd [] getEnv with
  | .ok result =>
    if result.hasFlag "verbose" then
      throw (IO.userError "Expected verbose flag to NOT be set")
    pure ()
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "help text includes env var info" := do
  let cmd := command "test" do
    Cmd.flag "token" (description := "API token") (envVar := some "API_TOKEN")
  let help := cmd.helpText
  shouldSatisfy (help.containsSubstr "[env: API_TOKEN]") "help should contain env var info"


end Tests.EnvVar

-- Repeatable Flag Tests
namespace Tests.RepeatableFlags

open Staple (String.containsSubstr)

testSuite "RepeatableFlags"

test "repeatable flag collects multiple values" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (short := some 'I') (description := "Include path")
  match parse cmd ["-I", "path1", "-I", "path2", "--include", "path3"] with
  | .ok result =>
    let values := result.getStrings "include"
    values ≡ ["path1", "path2", "path3"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "repeatable flag with long flag value syntax" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "define" (short := some 'D')
  match parse cmd ["--define=FOO", "--define=BAR"] with
  | .ok result =>
    let values := result.getStrings "define"
    values ≡ ["FOO", "BAR"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "non-repeatable flag replaces value" := do
  let cmd := command "test" do
    Cmd.flag "output" (short := some 'o')
  match parse cmd ["-o", "first", "-o", "second"] with
  | .ok result =>
    result.getString "output" ≡ some "second"
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "repeatable flag getInts" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "port" (argType := .int)
  match parse cmd ["--port", "80", "--port", "443", "--port", "8080"] with
  | .ok result =>
    let ports := result.getInts "port"
    ports ≡ [80, 443, 8080]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "repeatable flag empty when not provided" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include"
  match parse cmd [] with
  | .ok result =>
    let values := result.getStrings "include"
    values ≡ []
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "repeatable flag with default value" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (defaultValue := some "default")
  match parse cmd [] with
  | .ok result =>
    let values := result.getStrings "include"
    values ≡ ["default"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "repeatable flag overrides default when values provided" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (defaultValue := some "default")
  match parse cmd ["--include", "custom"] with
  | .ok result =>
    let values := result.getStrings "include"
    values ≡ ["custom"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "help text shows repeatable indicator" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (short := some 'I') (description := "Include path")
  let help := cmd.helpText
  shouldSatisfy (help.containsSubstr "[can repeat]") "help should indicate repeatable"

test "repeatable flag getAll with paths" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "path" (argType := .path)
  match parse cmd ["--path", "/usr/lib", "--path", "/usr/local/lib"] with
  | .ok result =>
    let paths := result.getPaths "path"
    paths.length ≡ 2
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "mixed flags - repeatable and non-repeatable" := do
  let cmd := command "test" do
    Cmd.flag "output"
    Cmd.repeatableFlag "include"
  match parse cmd ["--output", "out.txt", "--include", "a", "--include", "b"] with
  | .ok result =>
    result.getString "output" ≡ some "out.txt"
    result.getStrings "include" ≡ ["a", "b"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")

test "required repeatable flag validates" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (required := true)
  match parse cmd [] with
  | .ok _ => throw (IO.userError "Expected validation error")
  | .error e =>
    match e with
    | .missingRequired _ => pure ()
    | _ => throw (IO.userError s!"Expected missingRequired, got {e}")

test "required repeatable flag satisfied with one value" := do
  let cmd := command "test" do
    Cmd.repeatableFlag "include" (required := true)
  match parse cmd ["--include", "path1"] with
  | .ok result =>
    let values := result.getStrings "include"
    values ≡ ["path1"]
  | .error e => throw (IO.userError s!"Expected success, got error: {e}")


end Tests.RepeatableFlags

-- Table Tests
namespace Tests.Tables

testSuite "Tables"

test "Table.simple creation" := do
  let t := Output.Table.simple ["A", "B"] [["1", "2"], ["3", "4"]]
  t.columns.size ≡ 2
  t.rows.size ≡ 2


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


end Tests.Completion

-- REPL LineBuffer Tests
namespace Tests.LineBuffer

open Parlance.Repl

testSuite "LineBuffer"

test "empty buffer" := do
  let lb := LineBuffer.empty
  lb.content ≡ ""
  lb.cursor ≡ 0
  shouldSatisfy lb.isEmpty "empty buffer should be empty"

test "ofString creates buffer with cursor at end" := do
  let lb := LineBuffer.ofString "hello"
  lb.content ≡ "hello"
  lb.cursor ≡ 5

test "insertChar at end" := do
  let lb := LineBuffer.ofString "hel"
  let lb' := lb.insertChar 'l'
  lb'.content ≡ "hell"
  lb'.cursor ≡ 4

test "insertChar in middle" := do
  let lb : LineBuffer := { content := "hllo", cursor := 1 }
  let lb' := lb.insertChar 'e'
  lb'.content ≡ "hello"
  lb'.cursor ≡ 2

test "deleteBackward at end" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.deleteBackward
  lb'.content ≡ "hell"
  lb'.cursor ≡ 4

test "deleteBackward at start does nothing" := do
  let lb : LineBuffer := { content := "hello", cursor := 0 }
  let lb' := lb.deleteBackward
  lb'.content ≡ "hello"
  lb'.cursor ≡ 0

test "deleteForward in middle" := do
  let lb : LineBuffer := { content := "hello", cursor := 2 }
  let lb' := lb.deleteForward
  lb'.content ≡ "helo"
  lb'.cursor ≡ 2

test "deleteForward at end does nothing" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.deleteForward
  lb'.content ≡ "hello"
  lb'.cursor ≡ 5

test "moveCursorLeft" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.moveCursorLeft
  lb'.cursor ≡ 4

test "moveCursorLeft at start does nothing" := do
  let lb : LineBuffer := { content := "hello", cursor := 0 }
  let lb' := lb.moveCursorLeft
  lb'.cursor ≡ 0

test "moveCursorRight" := do
  let lb : LineBuffer := { content := "hello", cursor := 2 }
  let lb' := lb.moveCursorRight
  lb'.cursor ≡ 3

test "moveCursorRight at end does nothing" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.moveCursorRight
  lb'.cursor ≡ 5

test "moveCursorStart" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.moveCursorStart
  lb'.cursor ≡ 0

test "moveCursorEnd" := do
  let lb : LineBuffer := { content := "hello", cursor := 2 }
  let lb' := lb.moveCursorEnd
  lb'.cursor ≡ 5

test "deleteToEnd" := do
  let lb : LineBuffer := { content := "hello world", cursor := 5 }
  let lb' := lb.deleteToEnd
  lb'.content ≡ "hello"
  lb'.cursor ≡ 5

test "deleteToStart" := do
  let lb : LineBuffer := { content := "hello world", cursor := 6 }
  let lb' := lb.deleteToStart
  lb'.content ≡ "world"
  lb'.cursor ≡ 0

test "deleteWord simple" := do
  let lb : LineBuffer := { content := "hello world", cursor := 11 }
  let lb' := lb.deleteWord
  lb'.content ≡ "hello "
  lb'.cursor ≡ 6

test "beforeCursor and afterCursor" := do
  let lb : LineBuffer := { content := "hello world", cursor := 5 }
  lb.beforeCursor ≡ "hello"
  lb.afterCursor ≡ " world"

test "clear resets buffer" := do
  let lb := LineBuffer.ofString "hello"
  let lb' := lb.clear
  lb'.content ≡ ""
  lb'.cursor ≡ 0


end Tests.LineBuffer

-- REPL Input Tests
namespace Tests.ReplInput

open Parlance.Repl

testSuite "ReplInput"

test "KeyEvent constructors" := do
  let e := KeyEvent.char 'a'
  e.code ≡ KeyCode.char 'a'
  e.modifiers.ctrl ≡ false

test "KeyEvent withCtrl" := do
  let e := KeyEvent.char 'c' |>.withCtrl
  e.modifiers.ctrl ≡ true

test "KeyEvent isCtrlC" := do
  let e := KeyEvent.char 'c' |>.withCtrl
  shouldSatisfy e.isCtrlC "Ctrl+C should be detected"

test "KeyEvent isCtrlD" := do
  let e := KeyEvent.char 'd' |>.withCtrl
  shouldSatisfy e.isCtrlD "Ctrl+D should be detected"

test "KeyEvent isCtrlC negative" := do
  let e := KeyEvent.char 'c'
  shouldSatisfy (!e.isCtrlC) "c without Ctrl should not be Ctrl+C"

test "KeyCode toChar" := do
  let code := KeyCode.char 'x'
  code.toChar ≡ some 'x'

test "KeyCode toChar on non-char" := do
  let code := KeyCode.enter
  shouldBeNone code.toChar


end Tests.ReplInput

-- Markdown Tests
namespace Tests.Markdown

open Parlance.Markdown
open Staple (String.containsSubstr)

testSuite "Markdown"

test "plain text unchanged" :=
  render "hello world" ≡ "hello world"

test "bold renders with ANSI" := do
  let result := render "**hello**"
  shouldSatisfy (result.containsSubstr "\x1b[1m") "should contain bold code"
  shouldSatisfy (result.containsSubstr "hello") "should contain text"

test "italic star renders with ANSI" := do
  let result := render "*hello*"
  shouldSatisfy (result.containsSubstr "\x1b[3m") "should contain italic code"
  shouldSatisfy (result.containsSubstr "hello") "should contain text"

test "italic underscore renders with ANSI" := do
  let result := render "_hello_"
  shouldSatisfy (result.containsSubstr "\x1b[3m") "should contain italic code"
  shouldSatisfy (result.containsSubstr "hello") "should contain text"

test "inline code renders with ANSI" := do
  let result := render "`code`"
  shouldSatisfy (result.containsSubstr "code") "should contain text"
  -- Code uses cyan color
  shouldSatisfy (result.containsSubstr "\x1b[") "should contain ANSI escape"

test "header renders with ANSI" := do
  let result := render "# Header\n"
  shouldSatisfy (result.containsSubstr "Header") "should contain text"
  -- Headers use color + bold, check for ANSI escape sequence
  shouldSatisfy (result.containsSubstr "\x1b[") "should contain ANSI escape"

test "streaming handles split bold" := do
  let (s1, o1) := feed State.new "**hel"
  let (s2, o2) := feed s1 "lo**"
  let o3 := finish s2
  let result := o1 ++ o2 ++ o3
  shouldSatisfy (result.containsSubstr "hello") "should contain full text"

test "unclosed bold emits literally" := do
  let result := render "**unclosed"
  result ≡ "**unclosed"

test "mixed content" := do
  let result := render "This is **bold** and *italic* text."
  shouldSatisfy (result.containsSubstr "This is ") "should have plain text"
  shouldSatisfy (result.containsSubstr "bold") "should have bold text"
  shouldSatisfy (result.containsSubstr "italic") "should have italic text"
  shouldSatisfy (result.containsSubstr " text.") "should have trailing text"


end Tests.Markdown

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
