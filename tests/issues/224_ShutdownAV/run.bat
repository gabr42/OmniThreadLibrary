@echo off
rem Builds and runs the #224 repro in 32- and 64-bit mode. Exit code 0 = all as expected.
setlocal EnableDelayedExpansion
set DELPHI=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_issue224
mkdir %OUT%\w32 %OUT%\w64 2>nul
set NS=System;System.Win;Winapi;Vcl
"%DELPHI%\dcc32.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w32" "-NU%OUT%\w32" Issue224.dpr || exit /b 2
"%DELPHI%\dcc64.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w64" "-NU%OUT%\w64" Issue224.dpr || exit /b 2
for %%p in (w32 w64) do (
  echo --- %%p noleak
  %OUT%\%%p\Issue224.exe noleak
  echo --- %%p leak
  %OUT%\%%p\Issue224.exe leak
  echo exit code: !errorlevel!
)
