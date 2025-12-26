import Lake
open Lake DSL

package parlance where
  version := v!"0.1.0"

require crucible from ".." / "crucible"

@[default_target]
lean_lib Parlance where
  roots := #[`Parlance]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe parlance_tests where
  root := `Tests.Main
