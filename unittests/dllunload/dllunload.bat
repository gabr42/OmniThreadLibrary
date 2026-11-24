@echo off
rem Checks that a DLL that uses OTL can be unloaded. Windows only; needs Python.
rem   dllunload <compiler, for example e:\Delphi\37.0\bin\dcc32.exe> [repeat count]
if "%~1"=="" (echo Usage: dllunload ^<path to dcc32.exe or dcc64.exe^> [repeat] & exit /b 1)
set "OUT=%TEMP%\otl_dllunload"
mkdir "%OUT%" 2>nul
cd /d "%~dp0"
for %%p in (OtlUnloadDll UnloadHost) do (
  "%~1" %%p.dpr -B -q "-U..\..;..\..\src" "-I..\.." "-NSSystem;System.Win;Winapi;Vcl;Data;Xml" "-E%OUT%" "-NU%OUT%" || exit /b 1
)
python -I dllunload.py "%OUT%" %2
