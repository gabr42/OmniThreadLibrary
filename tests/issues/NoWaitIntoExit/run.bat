@echo off
rem Builds the repro in 32- and 64-bit mode and runs the failing case 10 times in each (the hang is a race).
setlocal EnableDelayedExpansion
set DELPHI=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_NoWaitIntoExit
mkdir %OUT%\w32 %OUT%\w64 2>nul
set NS=System;System.Win;Winapi;Vcl
"%DELPHI%\dcc32.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w32" "-NU%OUT%\w32" NoWaitIntoExit.dpr || exit /b 2
"%DELPHI%\dcc64.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\w64" "-NU%OUT%\w64" NoWaitIntoExit.dpr || exit /b 2
set RC=0
for %%p in (w32 w64) do (
  for /l %%i in (1,1,10) do (
    %OUT%\%%p\NoWaitIntoExit.exe exit collection ordered
    if errorlevel 1 set RC=1
  )
  %OUT%\%%p\NoWaitIntoExit.exe release collection ordered
  if errorlevel 1 set RC=1
  %OUT%\%%p\NoWaitIntoExit.exe exit range ordered
  if errorlevel 1 set RC=1
)
exit /b %RC%
