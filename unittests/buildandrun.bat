@echo off

  set otl_ut_root=c:\0

  mkdir %otl_ut_root% >nul 2>nul
  mkdir %otl_ut_root%\exe >nul 2>nul
  mkdir %otl_ut_root%\dcu >nul 2>nul
  mkdir %otl_ut_root%\dcu\Win32 >nul 2>nul
  mkdir %otl_ut_root%\dcu\Win64 >nul 2>nul

echo Compiling 32-bit
  dcc32 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win32 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
  if errorlevel 1 goto error

echo Running 32-bit
  %otl_ut_root%\exe\TestRunner >nul 2>nul
  if errorlevel 1 goto error

  dcc64 >nul 2>nul
  if errorlevel 1 goto exit

echo Compiling 64-bit  
  dcc64 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win64 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
  if errorlevel 1 goto error

  if not %PROCESSOR_ARCHITECTURE%==AMD64 goto on32bit

echo Running 64-bit
  %otl_ut_root%\exe\TestRunner >nul 2>nul
  if errorlevel 1 goto error

  goto exit

:on32bit
  echo Cannot run 64-bit test project on 32-bit windows.

:exit
  rem clear errorlevel
  dcc32 >nul 2>nul
  goto done

:error
  echo *** ERROR *** 
  type %otl_ut_root%\build.log

  rem set errorlevel
  VERIFY OTHER 2> NUL

:done
