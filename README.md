# Parlance

A command-line interface library for Lean 4.

Parlance provides argument parsing, styled output, and progress indicators for building CLI applications.

## Features

- **Argument Parsing**: Flags (`--verbose`, `-v`), options with values, positional args, subcommands
- **Auto Help Generation**: Automatic usage and help text from command definitions
- **Type-Safe Extraction**: Parse arguments directly to `String`, `Int`, `Nat`, `Float`, `Bool`, `FilePath`
- **Styled Output**: ANSI colors (16, 256, RGB), semantic helpers (error, warning, success, info)
- **Progress Indicators**: Animated spinners and progress bars
- **Table Output**: Aligned tabular data with optional borders

## Requirements

- Lean 4 v4.26.0

## Installation

Add Parlance to your `lakefile.lean`:

```lean
require parlance from git "https://github.com/your-username/parlance" @ "main"
```

Or clone and build locally:

```bash
git clone https://github.com/your-username/parlance
cd parlance
lake build
```

## Quick Start

```lean
import Parlance

open Parlance

def main (args : List String) : IO UInt32 := do
  let cli := command "myapp" do
    Cmd.version "1.0.0"
    Cmd.description "My awesome application"
    Cmd.boolFlag "verbose" (short := some 'v') (description := "Enable verbose output")
    Cmd.flag "output" (short := some 'o') (argType := .path) (description := "Output file")
    Cmd.arg "input" (description := "Input file to process")

  match parse cli args with
  | .ok result =>
    if result.hasFlag "verbose" then
      printInfo "Verbose mode enabled"

    let input := result.getString! "input"
    let output := result.getPath "output" |>.getD "output.txt"

    printSuccess s!"Processing {input} -> {output}"
    return 0

  | .error e =>
    printParseError e
    return 1
```

## Command Definition

### Basic Command

```lean
let cli := command "myapp" do
  Cmd.version "1.0.0"
  Cmd.description "Application description"
```

### Flags

```lean
-- Boolean flag (present/absent)
Cmd.boolFlag "verbose" (short := some 'v')

-- Flag with value
Cmd.flag "output" (argType := .path) (description := "Output file")
Cmd.flag "count" (argType := .nat) (defaultValue := some "10")
Cmd.flag "config" (argType := .string) (required := true)
```

### Positional Arguments

```lean
Cmd.arg "input" (description := "Input file")
Cmd.arg "target" (required := false) (defaultValue := some "default")
```

### Subcommands

```lean
let cli := command "git" do
  Cmd.description "Version control"

  Cmd.subcommand "clone" do
    Cmd.description "Clone a repository"
    Cmd.arg "url"

  Cmd.subcommand "commit" do
    Cmd.boolFlag "all" (short := some 'a')
    Cmd.flag "message" (short := some 'm')
```

## Parsing Results

```lean
match parse cli args with
| .ok result =>
  -- Boolean flags
  let verbose := result.hasFlag "verbose"

  -- String values
  let name := result.getString "name"        -- Option String
  let name := result.getString! "name"       -- String with default ""

  -- Typed extraction
  let count : Option Nat := result.get "count"
  let path : Option FilePath := result.getPath "output"

  -- Check subcommand
  match result.commandPath with
  | ["clone"] => handleClone result
  | ["commit"] => handleCommit result
  | _ => handleRoot result

| .error e =>
  printParseError e
```

## Styled Output

### Semantic Helpers

```lean
printError "Something went wrong"      -- Red, bold
printWarning "This might be a problem" -- Yellow
printSuccess "Operation completed"     -- Green
printInfo "Processing files..."        -- Blue
printHint "Try --help for more info"   -- Dim
```

### Custom Styling

```lean
open Parlance.Style

-- Direct styling
Style.println { text := "Custom styled text", style := Style.bold }

-- Build styled text
let styled := Text.error "Error: " ++ Text.bold "file not found"
Style.println styled

-- Available styles
Style.bold
Style.dim
Style.italic
Style.underline
{ fg := .red, bg := .white, modifier := { bold := true } }
```

### Colors

```lean
-- 16 ANSI colors
Color16.red, Color16.green, Color16.blue, ...

-- 256 indexed colors
Color.indexed 208

-- RGB true color
Color.rgb 255 128 0
```

## Progress Indicators

### Spinner

```lean
let spinner ← Spinner.create .dots "Loading..."

-- Update periodically
for _ in [:100] do
  spinner.tick
  IO.sleep 50

spinner.done "Complete!"
```

Available spinner styles: `.dots`, `.line`, `.braille`, `.arrows`, `.bounce`

### Progress Bar

```lean
let progress ← Progress.create 100 "Files"

for i in [:100] do
  progress.update (i + 1)
  IO.sleep 20

progress.done
```

## Table Output

```lean
open Parlance.Output

-- Simple table
let table := Table.simple
  ["Name", "Size", "Modified"]
  [["README.md", "1.2 KB", "2025-01-15"],
   ["src/main.lean", "4.5 KB", "2025-01-14"]]

table.print

-- With styling
let table := Table.withBorders
  [Column.new "Name" .left,
   Column.new "Size" .right,
   Column.new "Modified" .center]
  rows

table.print
```

## Auto-Generated Help

Commands automatically support `--help` and `-h`:

```
$ myapp --help
My awesome application

Usage: myapp [OPTIONS] <input>

Arguments:
  <input>                 Input file to process

Options:
  -v, --verbose           Enable verbose output
  -o, --output <PATH>     Output file
  -h, --help              Print help
  -V, --version           Print version
```

## Project Structure

```
parlance/
├── lakefile.lean
├── lean-toolchain
├── Parlance.lean              # Main import
├── Parlance/
│   ├── Core/
│   │   ├── Types.lean         # ArgType, Flag, Arg, Command, ParseResult
│   │   └── Error.lean         # ParseError types
│   ├── Parse/
│   │   ├── Tokenizer.lean     # CLI argument tokenization
│   │   ├── Parser.lean        # Core parsing logic
│   │   └── Extractor.lean     # Type-safe value extraction
│   ├── Command/
│   │   ├── Builder.lean       # CommandM monad for building commands
│   │   └── Help.lean          # Help text generation
│   ├── Style/
│   │   ├── Ansi.lean          # ANSI escape sequences
│   │   ├── Color.lean         # Color types (16/256/RGB)
│   │   ├── Modifier.lean      # Text modifiers (bold, dim, etc.)
│   │   ├── Styled.lean        # StyledText rendering
│   │   └── Semantic.lean      # error/warning/success/info helpers
│   └── Output/
│       ├── Spinner.lean       # Animated spinners
│       ├── Progress.lean      # Progress bars
│       └── Table.lean         # Tabular output
└── Tests/
    └── Main.lean              # Test suite
```

## Testing

```bash
lake test
```

## License

MIT License - see [LICENSE](LICENSE) for details.
