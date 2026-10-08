@echo off
rem Builds the #68 program with and without -DDSiNoTimerResolution (32-bit) and checks that only the
rem default build imports timeBeginPeriod from winmm.dll. The current timer resolution is system
rem wide, so it cannot be used for the check on a machine where other programs ask for 1 ms.
setlocal EnableDelayedExpansion
set DELPHI=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_Issue68
mkdir %OUT%\default %OUT%\nores 2>nul
set NS=System;System.Win;Winapi;Vcl
"%DELPHI%\dcc32.exe" -B -Q "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\default" "-NU%OUT%\default" Issue68.dpr || exit /b 2
"%DELPHI%\dcc32.exe" -B -Q -DDSiNoTimerResolution "-U..\..\..;..\..\..\src" "-NS%NS%" "-E%OUT%\nores" "-NU%OUT%\nores" Issue68.dpr || exit /b 2
set RC=0
findstr /c:"timeBeginPeriod" %OUT%\default\Issue68.exe >nul
if errorlevel 1 ( echo FAIL: default build does not call timeBeginPeriod & set RC=1 ) else echo PASS: default build calls timeBeginPeriod
findstr /c:"timeBeginPeriod" %OUT%\nores\Issue68.exe >nul
if errorlevel 1 ( echo PASS: DSiNoTimerResolution build does not call timeBeginPeriod ) else ( echo FAIL: DSiNoTimerResolution build calls timeBeginPeriod & set RC=1 )
exit /b %RC%
