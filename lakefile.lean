import Lake
open Lake DSL

package chisel where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.1"

@[default_target]
lean_lib Chisel where
  roots := #[`Chisel]

lean_lib Tests where
  globs := #[.submodules `Tests]

@[test_driver]
lean_exe chisel_tests where
  root := `Tests.Main
