@echo off
rem Builds the Issue40 repro in 32- and 64-bit mode and runs it (optionally with the arguments listed in CASES).
setlocal EnableDelayedExpansion
set DELPHI=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_Issue40
mkdir %OUT%\w32 %OUT%\w64 2>nul
set NS=System;System.Win;Winapi;Vcl
"%DELPHI%\dcc32.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w32" "-NU%OUT%\w32" Issue40.dpr || exit /b 2
"%DELPHI%\dcc64.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w64" "-NU%OUT%\w64" Issue40.dpr || exit /b 2
set RC=0
for %%p in (w32 w64) do (
  echo [%%p]
  %OUT%\%%p\Issue40.exe
  if errorlevel 1 set RC=1
)
exit /b %RC%
