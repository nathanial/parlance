/-
  Parlance.Repl.Terminal - Raw terminal FFI bindings

  Provides low-level terminal operations for REPL line editing:
  - Raw mode enable/disable (termios)
  - Non-blocking byte reading
  - Terminal size queries
-/
namespace Parlance.Repl.Terminal

/-- Enable raw terminal mode (no echo, no line buffering, no signals) -/
@[extern "parlance_repl_enable_raw_mode"]
opaque enableRawMode : IO Unit

/-- Disable raw terminal mode (restore original settings) -/
@[extern "parlance_repl_disable_raw_mode"]
opaque disableRawMode : IO Unit

/-- Get terminal size as (width, height) -/
@[extern "parlance_repl_get_size"]
opaque getTerminalSize : IO (Nat × Nat)

/-- Read a single byte from stdin (non-blocking, returns none if no input) -/
@[extern "parlance_repl_read_byte"]
opaque readByte : IO (Option UInt8)

/-- Execute an action with raw mode enabled, ensuring cleanup on exit -/
def withRawMode (action : IO α) : IO α := do
  enableRawMode
  try
    action
  finally
    disableRawMode

/-- Read a byte, blocking until one is available -/
partial def readByteBlocking : IO UInt8 := do
  match ← readByte with
  | some b => pure b
  | none =>
    IO.sleep 10  -- 10ms polling interval
    readByteBlocking

end Parlance.Repl.Terminal
