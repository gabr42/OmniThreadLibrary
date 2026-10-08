@echo off
rem Builds the Issue69 repro in 32- and 64-bit mode and runs each case in its own process.
setlocal EnableDelayedExpansion
set DELPHI=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_Issue69
mkdir %OUT%\w32 %OUT%\w64 2>nul
set NS=System;System.Win;Winapi;Vcl
"%DELPHI%\dcc32.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w32" "-NU%OUT%\w32" Issue69.dpr || exit /b 2
"%DELPHI%\dcc64.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w64" "-NU%OUT%\w64" Issue69.dpr || exit /b 2
set RC=0
for %%p in (w32 w64) do (
  for %%c in (ForLoopPumped NestedAsync NestedPipeline NestedAsyncTask) do (
    echo [%%p] %%c
    %OUT%\%%p\Issue69.exe %%c
    if errorlevel 1 set RC=1
  )
)
exit /b %RC%
