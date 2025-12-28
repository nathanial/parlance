import Lake
open Lake DSL

package parlance where
  version := v!"0.1.0"
  precompileModules := true

require crucible from ".." / "crucible"
require staple from ".." / "staple"

@[default_target]
lean_lib Parlance where
  roots := #[`Parlance]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe parlance_tests where
  root := `Tests.Main

-- Example REPL executable
lean_exe repl_demo where
  root := `examples.ReplDemo

-- FFI: Build C code for REPL terminal support
target repl_ffi.o pkg : System.FilePath := do
  let oFile := pkg.buildDir / "ffi" / "parlance_repl.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "parlance_repl.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs #["-fPIC"] "cc" getLeanTrace

extern_lib «parlance_repl_ffi» pkg := do
  let name := nameToStaticLib "parlance_repl_ffi"
  let ffiO ← repl_ffi.o.fetch
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]
