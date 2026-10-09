# OmniThreadLibrary

## Verify all supported compilers after every code change

OTL supports Delphi 2007 to Delphi 13 (see README.md). Each time code is modified (feature implemented,
code changed, bug fixed, ...), verify that it still compiles with ALL supported compilers before the work
is considered done or committed.

* Run `unittests\CompileAllUnits_All.bat`. It compiles `CompileAllUnits.dpr` with `dcc32` from
  `e:\Delphi\<ver>\bin` for all 19 versions. Success is `GLOBAL STATUS: OK`; each failing compiler leaves
  `unittests\compile_<env>.log`.
* Run it from PowerShell: `cd unittests; cmd /c .\CompileAllUnits_All.bat`. Calling it from Git Bash
  (`cmd //c`) fails.
* The batch does not create its output folders. They must exist, otherwise every compiler fails with
  F2039: `c:\0\MultiBuilder\<env name>\exe` and `c:\0\MultiBuilder\<env name>\dcu\win32`
  (`<env name>` as in the batch file, e.g. `Delphi 2007`, `Delphi 10.1 Berlin`).
* Delete the untracked `compile_*.log` files afterwards.

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
