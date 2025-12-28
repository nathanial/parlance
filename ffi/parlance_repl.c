// Parlance REPL FFI - Terminal operations via termios
// Adapted from terminus/ffi/terminus.c

#include <lean/lean.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>

// Store original terminal settings for restoration
static struct termios original_termios;
static int raw_mode_enabled = 0;

// Enable raw mode - disable canonical mode, echo, and signals
LEAN_EXPORT lean_obj_res parlance_repl_enable_raw_mode(lean_obj_arg world) {
    if (raw_mode_enabled) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (tcgetattr(STDIN_FILENO, &original_termios) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to get terminal attributes")));
    }

    struct termios raw = original_termios;

    // Input flags: disable break signal, CR to NL, parity check, strip 8th bit, XON/XOFF
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

    // Output flags: keep OPOST enabled so \n works normally (adds \r)
    // Unlike full TUI mode, REPL wants normal output processing

    // Control flags: set 8-bit chars
    raw.c_cflag |= (CS8);

    // Local flags: disable echo, canonical mode, signals, extended input
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

    // Control chars: non-blocking read (VMIN=0, VTIME=0 means return immediately)
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to set terminal to raw mode")));
    }

    raw_mode_enabled = 1;
    return lean_io_result_mk_ok(lean_box(0));
}

// Disable raw mode - restore original terminal settings
LEAN_EXPORT lean_obj_res parlance_repl_disable_raw_mode(lean_obj_arg world) {
    if (!raw_mode_enabled) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &original_termios) == -1) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to restore terminal settings")));
    }

    raw_mode_enabled = 0;
    return lean_io_result_mk_ok(lean_box(0));
}

// Get terminal size as (width, height)
LEAN_EXPORT lean_obj_res parlance_repl_get_size(lean_obj_arg world) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        // Fallback to default size
        ws.ws_col = 80;
        ws.ws_row = 24;
    }

    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box(ws.ws_col));
    lean_ctor_set(pair, 1, lean_box(ws.ws_row));

    return lean_io_result_mk_ok(pair);
}

// Read a single byte from stdin, returns Option UInt8
// Returns none if no byte available (non-blocking)
LEAN_EXPORT lean_obj_res parlance_repl_read_byte(lean_obj_arg world) {
    unsigned char c;
    ssize_t nread = read(STDIN_FILENO, &c, 1);

    if (nread == 1) {
        // Some c
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, lean_box(c));
        return lean_io_result_mk_ok(some);
    } else {
        // None
        return lean_io_result_mk_ok(lean_box(0));
    }
}
