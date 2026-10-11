# OmniThreadLibrary

## Verify all supported compilers after every code change

OTL supports Delphi 2007 to Delphi 13 (see README.md). Each time code is modified (feature implemented,
code changed, bug fixed, ...), verify that it still compiles with ALL supported compilers before the work
is considered done or committed.

* Run `unittests\CompileAllUnits_All.bat`. It compiles `CompileAllUnits.dpr` for all 19 versions with
  `dcc32` and, for Delphi XE2 and newer (the first with a 64-bit compiler), also with `dcc64`, using the
  compilers in `e:\Delphi\<ver>\bin`. Success is `GLOBAL STATUS: OK`.
* Run it from PowerShell: `cd unittests; cmd /c .\CompileAllUnits_All.bat`. Calling it from Git Bash
  (`cmd //c`) fails.
* The batch creates its output folders under `c:\0\MultiBuilder\<env name>\`.
* A failing compiler leaves `unittests\compile_<env>.log` (32-bit) or `compile_<env>_64.log` (64-bit);
  delete these untracked files afterwards.
* Both bitnesses matter: code behind `{$IFDEF CPUX64}`/`{$IFNDEF CPUX64}` is only compiled by the 64-bit
  run (a stray `const` left empty by a 32-bit-only guard once broke the 64-bit build unnoticed).

Conditional compilation is feature-based, not version-based: never test `CompilerVersion` or `VERxxx` in
the units. `OtlOptions.inc` is the only place that maps compiler versions to symbols (`OTL_GoodGenerics`,
`OTL_HasSystemThreading`, `OTL_Supports64Bit`, ...); the units test those symbols. When a new language or
RTL feature needs a guard, add a feature symbol to `OtlOptions.inc` (with its version threshold there) and
use it with `{$IFDEF}`.

Pitfalls that have broken old compilers:

* Generics (`Generics.Collections`, `TQueue<T>`, ...) need `{$IFDEF OTL_GoodGenerics}` (Delphi XE and newer).
* No pointer arithmetic on `PByte` (not available in Delphi 2007); cast through `NativeInt`.
* No inline variables.
* Declare newer WinAPI constants locally (for example `PF_XMMI64_INSTRUCTIONS_AVAILABLE`, missing in XE
  and older); the old Windows units do not have them.
* `TSpinLock` exists only in XE and newer.
