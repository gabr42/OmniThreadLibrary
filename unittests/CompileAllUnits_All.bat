@echo off
rem Compiles all OTL units with every supported Delphi: 32-bit (dcc32) with all of them,
rem 64-bit (dcc64) with Delphi XE2 and newer (the first version with a 64-bit compiler).
set "NS=System;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml"
set "ALLOK=1"

call :Compile "e:\Delphi\5.0\bin" "Delphi 2007"
call :Compile "e:\Delphi\6.0\bin" "Delphi 2009"
call :Compile "e:\Delphi\7.0\bin" "Delphi 2010"
call :Compile "e:\Delphi\8.0\bin" "Delphi XE"
call :Compile "e:\Delphi\9.0\bin" "Delphi XE2" 64
call :Compile "e:\Delphi\10.0\bin" "Delphi XE3" 64
call :Compile "e:\Delphi\11.0\bin" "Delphi XE4" 64
call :Compile "e:\Delphi\12.0\bin" "Delphi XE5" 64
call :Compile "e:\Delphi\14.0\bin" "Delphi XE6" 64
call :Compile "e:\Delphi\15.0\bin" "Delphi XE7" 64
call :Compile "e:\Delphi\16.0\bin" "Delphi XE8" 64
call :Compile "e:\Delphi\17.0\bin" "Delphi 10 Seattle" 64
call :Compile "e:\Delphi\18.0\bin" "Delphi 10.1 Berlin" 64
call :Compile "e:\Delphi\19.0\bin" "Delphi 10.2 Tokyo" 64
call :Compile "e:\Delphi\20.0\bin" "Delphi 10.3 Rio" 64
call :Compile "e:\Delphi\21.0\bin" "Delphi 10.4 Sydney" 64
call :Compile "e:\Delphi\22.0\bin" "Delphi 11 Alexandria" 64
call :Compile "e:\Delphi\23.0\bin" "Delphi 12 Athens" 64
call :Compile "e:\Delphi\37.0\bin" "Delphi 13 Florence" 64

echo.
echo ================================
if "%ALLOK%"=="1" (
  echo GLOBAL STATUS: OK
) else (
  echo GLOBAL STATUS: ERROR
)
echo ================================
goto :eof

:Compile
rem %1 = compiler bin folder, %2 = environment name, %3 = 64 to also compile for Win64
set "BINPATH=%~1"
set "ENVNAME=%~2"
set "OUTROOT=c:\0\MultiBuilder\%ENVNAME%"
set "LOGNAME=%ENVNAME: =_%"
mkdir "%OUTROOT%\exe" "%OUTROOT%\exe64" "%OUTROOT%\dcu\win32" "%OUTROOT%\dcu\win64" 2>nul

echo Compiling %ENVNAME% ...
"%BINPATH%\dcc32.exe" CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -ns%NS% -e"%OUTROOT%\exe" -n0"%OUTROOT%\dcu\win32" > "compile_%LOGNAME%.log" 2>&1
if errorlevel 1 (
  echo   [ERROR] %ENVNAME% - see compile_%LOGNAME%.log
  set "ALLOK=0"
) else (
  erase "compile_%LOGNAME%.log"
  echo   [OK] %ENVNAME%
)

if not "%~3"=="64" goto :eof
echo Compiling %ENVNAME% (64-bit) ...
"%BINPATH%\dcc64.exe" CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -ns%NS% -e"%OUTROOT%\exe64" -n0"%OUTROOT%\dcu\win64" > "compile_%LOGNAME%_64.log" 2>&1
if errorlevel 1 (
  echo   [ERROR] %ENVNAME% 64-bit - see compile_%LOGNAME%_64.log
  set "ALLOK=0"
) else (
  erase "compile_%LOGNAME%_64.log"
  echo   [OK] %ENVNAME% 64-bit
)
goto :eof
