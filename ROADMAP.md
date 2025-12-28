# Parlance Roadmap

This document outlines potential improvements, new features, and code cleanup opportunities for the Parlance CLI library.

---

## Feature Proposals

### [COMPLETED] Shell Completion Generation

**Status:** Implemented

**Description:** Generate shell completion scripts for Bash, Zsh, and Fish from command definitions.

**Implementation:**
- `Parlance/Completion/Core.lean` - Shell enum, escape helpers, flag/subcommand extraction
- `Parlance/Completion/Bash.lean` - Bash script generation with `compgen` and case statements
- `Parlance/Completion/Zsh.lean` - Zsh script generation with `_arguments` and `#compdef`
- `Parlance/Completion/Fish.lean` - Fish script generation with `complete -c`
- `Parlance/Completion.lean` - Top-level API (`generateScript`, `handleCompletionRequest`)

**Usage:**
```lean
def main (args : List String) : IO UInt32 := do
  let cmd := command "myapp" do
    Cmd.boolFlag "verbose" (short := some 'v')
    Cmd.flag "format" (argType := .choice ["json", "yaml"])

  -- Handle completion before parsing
  if let some action := Completion.handleCompletionRequest cmd "myapp" args then
    action
    return 0
  -- Normal parsing...
```

**User installation:**
```bash
myapp --generate-completion bash > ~/.bash_completion.d/myapp
myapp --generate-completion zsh > ~/.zsh/completions/_myapp
myapp --generate-completion fish > ~/.config/fish/completions/myapp.fish
```

**Features:**
- Completes command names, subcommand names, flag names (long and short)
- `ArgType.path` triggers file completion in all shells
- `ArgType.choice` provides enumerated value completions
- `ArgType.bool` suggests true/false/yes/no values
- Nested subcommand support with per-subcommand flag completions

---

### [Priority: High] Environment Variable Support

**Description:** Allow flags and arguments to fall back to environment variables when not specified on the command line.

**Rationale:** Many CLI tools support configuration via environment variables (e.g., `--token` falls back to `$TOKEN`). This is essential for containerized deployments and CI/CD pipelines.

**Affected Files:**
- `Parlance/Core/Types.lean` (add `envVar : Option String` to Flag and Arg)
- `Parlance/Parse/Parser.lean` (check env vars before applying defaults)
- `Parlance/Command/Help.lean` (display env var in help text)

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: High] Multi-Value Flag Support

**Description:** Support flags that can be specified multiple times to collect multiple values (e.g., `-I path1 -I path2`).

**Rationale:** This is a common pattern in compilers, build tools, and many other CLI applications. The current implementation notes this limitation in `ParseResult.getList`.

**Affected Files:**
- `Parlance/Core/Types.lean` (add `repeatable : Bool` to Flag)
- `Parlance/Parse/Parser.lean` (accumulate values instead of replacing)
- `Parlance/Parse/Extractor.lean` (enhance `getList` to return actual collected values)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] REPL Support

**Description:** Add support for building REPL-style interactive applications (like `irb`, `ipython`, `psql`, `node`).

**Rationale:** There's a gap between batch CLIs (parse args → run → exit) and full TUI applications (raw mode screen takeover). Many tools need the middle ground: a prompt-based read-eval-print loop with line editing, history, and completion—but without full screen control. Examples include database clients (`psql`, `sqlite3`, `redis-cli`), language REPLs (`irb`, `ipython`, `ghci`, `node`), and interactive shells.

Parlance is the natural home for this since it already has:
- Command/argument definitions (reusable for REPL commands)
- Styled output infrastructure
- Completion definitions (can inform in-REPL tab completion)

| Level | Example | Description |
|-------|---------|-------------|
| Batch CLI | `grep`, `curl` | Parse args → run → exit |
| **REPL** | `irb`, `psql` | **Prompt → readline → eval → loop** |
| Full TUI | `vim`, `htop` | Raw mode takeover, widgets, full screen |

**Key Features:**
- **Line editing** - cursor movement (arrows, home/end), insert/delete, kill ring (ctrl+k/y)
- **History** - up/down navigation, ctrl+r incremental search, file persistence
- **Tab completion** - context-aware, uses existing Parlance completion infrastructure
- **Custom prompt** - can display state (current directory, connection info, etc.)
- **Normal stdout** - output scrolls naturally, coexists with scrollback

**Affected Files:**
- New file: `Parlance/Repl/Readline.lean` - line editing, cursor movement, key handling
- New file: `Parlance/Repl/History.lean` - in-memory history, file persistence, search
- New file: `Parlance/Repl/Completion.lean` - tab completion integration
- New file: `Parlance/Repl/Loop.lean` - REPL driver, eval dispatch
- New file: `Parlance/Repl/Prompt.lean` - customizable prompt rendering
- New file: `ffi/readline.c` - raw terminal input (partial raw mode for keypresses)

**Proposed API:**
```lean
import Parlance

open Parlance.Repl

def main : IO Unit := do
  let repl := Repl.create do
    Repl.prompt "> "
    Repl.historyFile ".myapp_history"
    Repl.maxHistory 1000

    -- Reuse Parlance command definitions for completion
    Repl.commands myCommandSpec

    -- Or custom completion
    Repl.completer fun line pos => do
      -- return completion candidates
      pure ["help", "quit", "load", "save"]

  repl.run fun input => do
    -- Eval the input, return whether to continue
    match input.trim with
    | "quit" => pure false
    | "help" => IO.println "Available commands: ..."; pure true
    | cmd =>
      -- Process command
      IO.println s!"You entered: {cmd}"
      pure true
```

**Estimated Effort:** Large

**Dependencies:**
- Raw terminal mode FFI (simpler than terminus - only needs keypress reading, not full screen control)
- Could potentially share terminal FFI code with terminus

---

### [Priority: Medium] Interactive Prompts

**Description:** Add support for interactive prompts when required values are missing (confirmation, text input, selection).

**Rationale:** Modern CLI tools like npm, gh, and pnpm provide interactive fallbacks when arguments are missing, improving UX for interactive use.

**Affected Files:**
- New file: `Parlance/Prompt/Text.lean`
- New file: `Parlance/Prompt/Confirm.lean`
- New file: `Parlance/Prompt/Select.lean`
- New file: `Parlance/Prompt/Password.lean`

**Estimated Effort:** Medium

**Dependencies:** REPL Support (can reuse readline infrastructure)

---

### [Priority: Medium] Configuration File Support

**Description:** Allow CLI arguments to be read from configuration files (TOML, JSON, YAML).

**Rationale:** Many CLI tools support config files (e.g., `.prettierrc`, `cargo.toml`). This enables persistent configuration without command-line clutter.

**Affected Files:**
- New file: `Parlance/Config/Loader.lean`
- New file: `Parlance/Config/Toml.lean` (basic TOML parser)
- `Parlance/Core/Types.lean` (add config file path option to Command)
- `Parlance/Parse/Parser.lean` (merge config file values)

**Estimated Effort:** Large

**Dependencies:** None (could optionally integrate with a TOML library)

---

### [Priority: Medium] Flag Groups and Mutual Exclusion

**Description:** Support grouping related flags and defining mutually exclusive flag sets.

**Rationale:** Many commands have flags that should not be used together (e.g., `--json` vs `--table` for output format). Expressing this declaratively improves UX and validation.

**Affected Files:**
- `Parlance/Core/Types.lean` (add `FlagGroup` type, add `group : Option String` to Flag)
- `Parlance/Parse/Parser.lean` (validate mutual exclusion after parsing)
- `Parlance/Core/Error.lean` (add `conflictingFlags` error variant)
- `Parlance/Command/Help.lean` (group flags visually in help output)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Negatable Boolean Flags

**Description:** Automatically support `--no-<flag>` variants for boolean flags.

**Rationale:** POSIX convention supports negatable flags (e.g., `--color` / `--no-color`). This is expected behavior in many CLI tools.

**Affected Files:**
- `Parlance/Core/Types.lean` (add `negatable : Bool` to Flag)
- `Parlance/Parse/Parser.lean` (handle `--no-` prefix)
- `Parlance/Command/Help.lean` (display negatable form)

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Colored Help Output

**Description:** Use ANSI colors in generated help text for better readability.

**Rationale:** The library already has comprehensive color support but help text is rendered as plain text. Colored help is standard in modern CLI tools.

**Affected Files:**
- `Parlance/Command/Help.lean` (use Style types instead of plain strings)

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Flag Value Completion Hints

**Description:** Allow flags to specify valid values or completion patterns for shell completion.

**Rationale:** Shell completions can suggest valid values (file paths, enum values, etc.) if the CLI provides hints.

**Current State:** Basic completion hints are already supported via `ArgType`:
- `ArgType.path` - triggers file completion
- `ArgType.choice` - provides enumerated values
- `ArgType.bool` - suggests true/false/yes/no

**Proposed Enhancement:** Add explicit `CompletionHint` type for advanced patterns:
- Directory-only completion
- Custom shell commands for dynamic completions
- Glob patterns for file filtering

**Affected Files:**
- `Parlance/Core/Types.lean` (add `completionHint : Option CompletionHint` to Flag)
- `Parlance/Completion/*.lean` (handle new hint types)

**Estimated Effort:** Small

**Dependencies:** ✓ Shell Completion Generation (completed)

---

### [Priority: Low] Man Page Generation

**Description:** Generate man pages from command definitions.

**Rationale:** Unix systems expect man pages for CLI tools. Automatic generation ensures documentation stays in sync with the code.

**Affected Files:**
- New file: `Parlance/Doc/Man.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Markdown Documentation Generation

**Description:** Generate Markdown documentation from command definitions.

**Rationale:** Automatically generated docs can be published to GitHub wikis, README files, or documentation sites.

**Affected Files:**
- New file: `Parlance/Doc/Markdown.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] Animated Progress with Threading

**Description:** Implement proper animated spinners using background threads.

**Rationale:** The current `withAnimatedSpinner` is a stub that doesn't actually animate. Real spinners require concurrent updates.

**Affected Files:**
- `Parlance/Output/Spinner.lean` (implement with Lean 4 tasks)

**Estimated Effort:** Medium

**Dependencies:** Lean 4 concurrency primitives (Task/IO.asTask)

---

### [Priority: Low] Multi-Progress Display

**Description:** Support displaying multiple progress bars simultaneously (like npm or cargo downloads).

**Rationale:** Useful for parallel operations where each task has its own progress indicator.

**Affected Files:**
- New file: `Parlance/Output/MultiProgress.lean`
- May need cursor positioning beyond single-line

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Log-Aware Progress

**Description:** Progress bars that can handle interleaved log output without corruption.

**Rationale:** When progress bars and log messages are printed simultaneously, output can become garbled. A coordinated output manager solves this.

**Affected Files:**
- New file: `Parlance/Output/OutputManager.lean`

**Estimated Effort:** Large

**Dependencies:** Threading support

---

---

## Code Improvements

### [Priority: High] Float Parsing in FromArg

**Current State:** The `FromArg Float` instance uses `s.toNat?.map Float.ofNat` which only parses integer values, not actual floats.

**Proposed Change:** Implement proper float string parsing that handles decimal points, negative numbers, and scientific notation.

**Benefits:** Correct behavior for float arguments

**Affected Files:**
- `Parlance/Parse/Extractor.lean` (lines 29-30)

**Estimated Effort:** Small

---

### [Priority: High] Type-Safe Argument Extraction

**Current State:** Argument extraction uses string-based lookups (e.g., `result.get "verbose"`), which is error-prone.

**Proposed Change:** Consider a type-safe extraction pattern using phantom types or dependent types to ensure argument names match at compile time.

**Benefits:** Compile-time safety, better refactoring support

**Affected Files:**
- `Parlance/Parse/Extractor.lean`
- `Parlance/Core/Types.lean`

**Estimated Effort:** Large

---

### [Priority: Medium] Short Flag Value Parsing

**Current State:** The tokenizer comment (line 68-69) notes: "A smarter tokenizer would handle -ovalue style". Currently `-ovalue` is tokenized as multiple short flags.

**Proposed Change:** Implement proper handling of short flags with attached values (e.g., `-o file` and `-ofile` should both work).

**Benefits:** Better POSIX compliance, more natural CLI syntax

**Affected Files:**
- `Parlance/Parse/Tokenizer.lean` (lines 66-72)
- `Parlance/Parse/Parser.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Improved Choice Validation

**Current State:** Choice arguments (`ArgType.choice`) are parsed but validation against valid options is not enforced in the parser.

**Proposed Change:** Add choice validation during parsing and generate proper `invalidChoice` errors.

**Benefits:** Better error messages, fail-fast behavior

**Affected Files:**
- `Parlance/Parse/Parser.lean`
- `Parlance/Parse/Extractor.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] ParsedValues Efficiency

**Current State:** `ParsedValues` uses `List (String x String)` with linear search and append operations.

**Proposed Change:** Use `Std.HashMap` or `Lean.RBMap` for O(log n) or O(1) lookups.

**Benefits:** Better performance for commands with many flags/arguments

**Affected Files:**
- `Parlance/Core/Types.lean` (ParsedValues structure)

**Estimated Effort:** Small

---

### [Priority: Medium] Builder Pattern Consistency

**Current State:** Both `CommandM` methods and `Cmd` namespace methods exist, with `Cmd` re-exporting `CommandM` methods.

**Proposed Change:** Consider deprecating one approach or documenting the recommended pattern clearly.

**Benefits:** Clearer API, reduced confusion

**Affected Files:**
- `Parlance/Command/Builder.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Modifier.toCodes Efficiency

**Current State:** `Modifier.toCodes` uses repeated list concatenation with `++`.

**Proposed Change:** Build the list in reverse and use a single reverse at the end, or use Array.

**Benefits:** O(n) instead of O(n^2) string building

**Affected Files:**
- `Parlance/Style/Modifier.lean` (lines 52-62)

**Estimated Effort:** Small

---

### [Priority: Low] Table Column Width Calculation

**Current State:** Column widths are calculated by iterating through all rows for each column.

**Proposed Change:** Calculate all column widths in a single pass through the data.

**Benefits:** Better performance for large tables (O(rows * cols) instead of O(rows * cols * cols))

**Affected Files:**
- `Parlance/Output/Table.lean` (columnWidths function)

**Estimated Effort:** Small

---

### [Priority: Low] Unicode-Aware String Length

**Current State:** String length calculations use `String.length` which counts code units.

**Proposed Change:** Use grapheme cluster counting for accurate visual width calculation (especially for emoji and CJK characters).

**Benefits:** Correct alignment in tables and help text for non-ASCII content

**Affected Files:**
- `Parlance/Output/Table.lean`
- `Parlance/Command/Help.lean`

**Estimated Effort:** Medium

---

---

## Code Cleanup

### [Priority: High] Remove Partial Annotation

**Issue:** `ParserM.parseAll` is marked `partial` due to recursive structure.

**Location:** `Parlance/Parse/Parser.lean` (line 181)

**Action Required:** Refactor to use well-founded recursion on token list length, removing the need for `partial`.

**Estimated Effort:** Small

---

### [Priority: Medium] Inconsistent Naming

**Issue:** Mixed naming conventions: `boolFlag` vs `findFlagLong`, `isBoolean` vs `hasBool`.

**Location:** Multiple files in `Parlance/Core/` and `Parlance/Command/`

**Action Required:** Establish and document naming conventions, update for consistency.

**Estimated Effort:** Small

---

### [Priority: Medium] Missing Documentation Comments

**Issue:** Many public functions lack doc comments explaining their purpose and usage.

**Location:** Throughout the codebase, particularly:
- `Parlance/Parse/Parser.lean` (ParserM methods)
- `Parlance/Output/` (spinner/progress operations)

**Action Required:** Add comprehensive doc comments to all public APIs.

**Estimated Effort:** Medium

---

### [Priority: Medium] Test Coverage Gaps

**Issue:** Several areas lack test coverage:
- Subcommand parsing
- Flag value type validation
- Progress and Spinner rendering
- Table border styles
- Default value application
- Required argument validation

**Location:** `Tests/Main.lean`

**Action Required:** Add comprehensive tests for all parsing edge cases and output formatting.

**Estimated Effort:** Medium

---

### [Priority: Low] Duplicate Padding Logic

**Issue:** Similar padding/alignment logic appears in multiple places.

**Location:**
- `Parlance/Command/Help.lean` (formatFlag, formatArg, formatSubcmd)
- `Parlance/Output/Table.lean` (pad function)

**Action Required:** Extract common padding/formatting utilities to a shared module.

**Estimated Effort:** Small

---

### [Priority: Low] Magic Numbers

**Issue:** Hard-coded values without named constants.

**Location:**
- `Parlance/Command/Help.lean`: `descColumn := 24`, `width := 80`
- `Parlance/Output/Progress.lean`: `width := 40`

**Action Required:** Define named constants or make configurable with sensible defaults.

**Estimated Effort:** Small

---

### [Priority: Low] Unused Token Variants

**Issue:** `Token.shortFlagValue` appears to be constructed but the tokenizer never produces it (comment says "A smarter tokenizer would handle -ovalue style").

**Location:** `Parlance/Parse/Tokenizer.lean` (line 16)

**Action Required:** Either implement short flag value tokenization or remove the unused variant.

**Estimated Effort:** Small

---

### [Priority: Low] Error Message Consistency

**Issue:** Some error messages include "Error:" prefix, others don't. Style varies.

**Location:** `Parlance/Core/Error.lean`, `Parlance/Style/Semantic.lean`

**Action Required:** Establish consistent error message format and style.

**Estimated Effort:** Small

---

---

## API Enhancements

### [Priority: High] Declarative Validation

**Description:** Add support for custom validation functions on arguments.

**Current State:** Validation is limited to type parsing. Custom validation (e.g., port range, file existence) must be done after parsing.

**Proposed API:**
```lean
Cmd.flag "port" (argType := .nat)
  (validate := fun n => if n < 65536 then .ok else .error "Port must be < 65536")
```

**Affected Files:**
- `Parlance/Core/Types.lean`
- `Parlance/Parse/Parser.lean`

**Estimated Effort:** Medium

---

### [Priority: High] Automatic Program Name Detection

**Description:** Automatically detect program name from `argv[0]` instead of requiring explicit specification.

**Current State:** Program name must be passed to `command` builder.

**Proposed API:**
```lean
def main (args : List String) : IO Unit := do
  let result <- parseArgs myCommand  -- Uses System.programName automatically
```

**Affected Files:**
- `Parlance/Parse/Parser.lean`
- New convenience functions

**Estimated Effort:** Small

---

### [Priority: Medium] Fluent Command Builder

**Description:** Add method chaining style for command building as an alternative to the monad.

**Current State:** Only monadic builder (`do ... Cmd.flag ...`) is available.

**Proposed API:**
```lean
Command.new "myapp"
  |>.withVersion "1.0"
  |>.withFlag (Flag.long "verbose" |>.short 'v' |>.description "Verbose output")
  |>.withArg (Arg.named "file" |>.required)
```

**Affected Files:**
- `Parlance/Core/Types.lean` (add fluent methods)
- New file or extend `Parlance/Command/Builder.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Result Monad for Parsing

**Description:** Provide a monad for handling parse results with automatic help/version printing.

**Current State:** User must handle `Except ParseError ParseResult` and check for help/version errors manually.

**Proposed API:**
```lean
def main (args : List String) : IO UInt32 := do
  let result <- runCli myCommand args  -- Auto-prints help/version, exits with code
  -- result is only the success case
```

**Affected Files:**
- New file: `Parlance/Run.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Derived Commands from Types

**Description:** Use Lean metaprogramming to derive CLI commands from structure types.

**Proposed API:**
```lean
structure Options where
  verbose : Bool := false
  output : System.FilePath
  count : Nat := 1
  deriving Command
```

**Affected Files:**
- New file: `Parlance/Derive.lean`

**Estimated Effort:** Large

---

---

## Summary

### Completed
1. ✓ Shell completion generation (Bash, Zsh, Fish)

### Quick Wins (Small Effort, High Impact)
1. Fix Float parsing in FromArg
2. Environment variable support
3. Negatable boolean flags
4. Remove `partial` from parseAll
5. Colored help output

### Medium-Term Improvements
1. Multi-value flag support
2. Short flag value parsing (-ovalue)
3. Flag groups and mutual exclusion
4. Comprehensive test coverage
5. Advanced completion hints (directories, custom commands)
6. Interactive prompts (one-shot: confirm, text input, select)

### Long-Term Features
1. **REPL support** (readline, history, in-REPL completion)
2. Configuration file support
3. Type-safe argument extraction with compile-time checking
4. Derived commands from types
5. PowerShell completion support
