@echo off

  call :build 5.0 2007
  call :build 9.0 XE2
  call :build 12.0 XE5
  call :build 18.0 10_1Berlin
  call :build 19.0 10_2Tokyo

  goto exit

:build
  set path=d:\delphi\%1\bin
  
  set otl_ut_root=c:\0\otl\%2
  mkdir c:\0\otl >nul 2>nul
  mkdir c:\0\otl\%2 >nul 2>nul
  
  pushd h:\razvoj\omnithreadlibrary
  echo.
  echo %2
  start "%2" cmd.exe /k doalltests %2
  popd
  goto :eof

:exit
