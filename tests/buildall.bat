@echo off
  Setlocal EnableDelayedExpansion

  if %1.==. goto usage

  if not %otl_ut_root%.==. goto makedir  
  set otl_ut_root=c:\0

:makedir
  mkdir %otl_ut_root% >nul 2>nul
  mkdir %otl_ut_root%\exe >nul 2>nul
  mkdir %otl_ut_root%\dcu >nul 2>nul
  mkdir %otl_ut_root%\dcu\Win32 >nul 2>nul
  mkdir %otl_ut_root%\dcu\Win64 >nul 2>nul  

  set _comp_fmx=1
  if /i %1==XE2 set _comp_fmx=0
  if /i %1==XE3 set _comp_fmx=0
  if /i %1==XE4 set _comp_fmx=0
  if /i %1==XE5 set _comp_fmx=0
  if /i %1==XE6 set _comp_fmx=0

  dcc64 >nul 2>nul
  if errorlevel 1 goto no64
  
  set hasdcc64=1
  goto compile
  
:no64
  set hasdcc64=0
  
:compile
  for /d %%a in (*) do (
    pushd %%a
    call :compile_one %1 %%a
    if errorlevel 1 goto error
    popd
  )

  echo All done %DATE%_%TIME%
  goto exit

:compile_one
  set c=

  for %%a in (*.%1.dproj) do (
    set c=1
  )                   

  for %%a in (*.%1.test) do (
    set c=1
  )       
  if %c%.==1. (
    for %%b in (*.dpr) do (
      if %%~xb==.dpr (
        call :compile %%b
        if errorlevel 1 exit /b 1
      )  
    )
  ) else (
    echo %2: Missing
    rem clear errorlevel
    dcc32 >nul 2>&1
  )

  goto :eof
  
:compile
  set _b1=%1
  set _b2=%_b1:FMX=%
  if not %_b1%==%_b2% (
      if %_comp_fmx%==0 goto :eof
  )
  
  if exist %otl_ut_root%\build.log del %otl_ut_root%\build.log >nul 2>nul
  dcc32 /b /u..\..;..\..\GpDelphiUnits\src;..\..\..\fastmm /i..\.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win32 %1 >%otl_ut_root%\build.log 2>&1
  if errorlevel 1 (
    echo %1[32]: *** ERROR *** 
    type %otl_ut_root%\build.log
    exit /b 1
  ) else (
    echo %1[32]: OK
  )
  if %hasdcc64%==1 (
    if exist %otl_ut_root%\build.log del %otl_ut_root%\build.log >nul 2>nul
    dcc64 /b /u..\..;..\..\GpDelphiUnits\src;..\..\..\fastmm /i..\.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win64 %1 >%otl_ut_root%\build.log 2>&1
    if errorlevel 1 (
      echo %1[64]: *** ERROR *** 
      type %otl_ut_root%\build.log
      exit /b 1
    ) else (
      echo %1[64]: OK
    )
  )
  goto :eof

:usage
  echo Usage:
  echo   buildall delphiVersion
  echo.
  echo Examples:
  echo   buildall 2007
  echo   buildall XE5
  goto exit

:error
  echo ERROR

:exit
  set c=
  set hasdcc64=
