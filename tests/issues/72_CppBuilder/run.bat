@echo off
rem Issue #72: the C++ headers generated for OmniThreadLibrary must compile in C++Builder, and a C++
rem program must be able to use the Delphi objects. Needs C++Builder (bcc32c, bcc64) next to the Delphi compilers.
rem   1. dcc32/dcc64 -JPHNE -DBCB generate .hpp/.obj for the library units
rem   2. every .hpp is compiled on its own (32- and 64-bit)
rem   3. UseOtl.cpp (critical section, resource count, value container) is linked with the objects and run (32-bit)
setlocal EnableDelayedExpansion
set BIN=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set OUT=%TEMP%\otl_issue72
set ROOT=%~dp0..\..\..
set UNITS=src\DSiWin32 src\GpStuff src\GpLists OtlCollections OtlComm OtlCommon.Utils OtlCommon OtlContainerObserver OtlContainers OtlDataManager OtlEventMonitor OtlHooks OtlLogger OtlParallel OtlRegister OtlSync.Utils OtlSync OtlTask OtlTaskControl OtlThreadPool
set HEADERS=DSiWin32 GpStuff GpLists OtlCollections OtlComm OtlCommon OtlContainerObserver OtlContainers OtlDataManager OtlEventMonitor OtlHooks OtlLogger OtlParallel OtlRegister OtlSync OtlTask OtlTaskControl OtlThreadPool
rmdir /s /q %OUT% 2>nul
mkdir %OUT%\hpp32 %OUT%\dcu32 %OUT%\obj32 %OUT%\hpp64 %OUT%\dcu64 %OUT%\obj64 %OUT%\work
set NS=System;System.Win;Winapi;Vcl
pushd %ROOT%
for %%u in (%UNITS%) do (
  "%BIN%\dcc32.exe" -B -Q -JPHNE -DBCB "-U.;src" -I. "-NS%NS%" "-NU%OUT%\dcu32" "-NH%OUT%\hpp32" "-NO%OUT%\obj32" "-N0%OUT%\dcu32" %%u.pas || ( popd & exit /b 2 )
  "%BIN%\dcc64.exe" -B -Q -JPHNE -DBCB "-U.;src" -I. "-NS%NS%" "-NU%OUT%\dcu64" "-NH%OUT%\hpp64" "-NO%OUT%\obj64" "-N0%OUT%\dcu64" %%u.pas || ( popd & exit /b 2 )
)
copy /y OtlEventMonitor.dcr %OUT%\work >nul
popd
set RC=0
pushd %OUT%\work
for %%h in (%HEADERS%) do (
  echo #pragma hdrstop> hdr_%%h.cpp
  echo #include ^<System.hpp^>>> hdr_%%h.cpp
  echo #include "%%h.hpp">> hdr_%%h.cpp
  "%BIN%\bcc32c.exe" -c -I..\hpp32 -o hdr_%%h.32.o hdr_%%h.cpp >nul 2>&1
  if errorlevel 1 ( echo FAIL: %%h.hpp does not compile ^(32-bit^) & set RC=1 ) else echo PASS: %%h.hpp ^(32-bit^)
  "%BIN%\bcc64.exe" -c -I..\hpp64 -o hdr_%%h.64.o hdr_%%h.cpp >nul 2>&1
  if errorlevel 1 ( echo FAIL: %%h.hpp does not compile ^(64-bit^) & set RC=1 ) else echo PASS: %%h.hpp ^(64-bit^)
)
copy /y %~dp0UseOtl.cpp . >nul
"%BIN%\bcc32c.exe" -tM -I..\hpp32 -o UseOtl.exe UseOtl.cpp ..\obj32\*.obj rtl.lib >nul 2>&1
if errorlevel 1 ( echo FAIL: UseOtl.cpp does not build & set RC=1 ) else (
  .\UseOtl.exe
  if errorlevel 1 ( echo FAIL: UseOtl.exe & set RC=1 ) else echo PASS: UseOtl.exe
)
popd
exit /b %RC%
