@echo off
  Setlocal EnableDelayedExpansion

  if %1.==. goto usage

  for /d %%a in (*) do (
    pushd %%a
    call :compile_one %1 %%a
    popd
  )

  goto exit

:compile_one
  set c=

  for %%a in (*.%1.dproj) do (
    set c=1
  )

  if %c%.==1. (
    for %%b in (*.dpr) do (
      if %%~xb==.dpr (  
        dcc32 /b /u..\..;..\..\src;..\..\fastmm /i..\.. %%b >nul 2>nul
        if errorlevel 1 (
          echo %2: *** ERROR *** 
        ) else (
          echo %2: OK
        )
      )
    )
  ) else (
    echo %2: Missing
  )

  goto :eof

:usage
  echo Usage:
  echo   buildall delphiVersion
  echo.
  echo Examples:
  echo   buildall 2007
  echo   buildall XE5

:exit
  set c=
