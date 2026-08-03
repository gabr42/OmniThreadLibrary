@echo off
set "NS=System;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml"
set "ALLOK=1"

call :Compile "e:\Delphi\5.0\bin" "Delphi 2007"
call :Compile "e:\Delphi\6.0\bin" "Delphi 2009"
call :Compile "e:\Delphi\7.0\bin" "Delphi 2010"
call :Compile "e:\Delphi\8.0\bin" "Delphi XE"
call :Compile "e:\Delphi\9.0\bin" "Delphi XE2"
call :Compile "e:\Delphi\10.0\bin" "Delphi XE3"
call :Compile "e:\Delphi\11.0\bin" "Delphi XE4"
call :Compile "e:\Delphi\12.0\bin" "Delphi XE5"
call :Compile "e:\Delphi\14.0\bin" "Delphi XE6"
call :Compile "e:\Delphi\15.0\bin" "Delphi XE7"
call :Compile "e:\Delphi\16.0\bin" "Delphi XE8"
call :Compile "e:\Delphi\17.0\bin" "Delphi 10 Seattle"
call :Compile "e:\Delphi\18.0\bin" "Delphi 10.1 Berlin"
call :Compile "e:\Delphi\19.0\bin" "Delphi 10.2 Tokyo"
call :Compile "e:\Delphi\20.0\bin" "Delphi 10.3 Rio"
call :Compile "e:\Delphi\21.0\bin" "Delphi 10.4 Sydney"
call :Compile "e:\Delphi\22.0\bin" "Delphi 11 Alexandria"
call :Compile "e:\Delphi\23.0\bin" "Delphi 12 Athens"
call :Compile "e:\Delphi\37.0\bin" "Delphi 13 Florence"

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
set "BINPATH=%~1"
set "ENVNAME=%~2"
set "LOGNAME=%ENVNAME: =_%"
set "LOGFILE=compile_%LOGNAME%.log"
echo Compiling %ENVNAME% ...
"%BINPATH%\dcc32.exe" CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -ns%NS% -e"c:\0\MultiBuilder\%ENVNAME%\exe" -n0"c:\0\MultiBuilder\%ENVNAME%\dcu\win32" > "%LOGFILE%" 2>&1
if errorlevel 1 (
  echo   [ERROR] %ENVNAME% - see %LOGFILE%
  set "ALLOK=0"
) else (
  erase "%LOGFILE%"
  echo   [OK] %ENVNAME%
)
goto :eof
