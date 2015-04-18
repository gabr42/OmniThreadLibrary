@echo off
  if %1.==. goto usage

  pushd unittests
  call buildandrun
  if errorlevel 1 goto exit
  popd

  pushd tests
  call buildall %1
  popd

  goto exit

:usage
  echo Usage:
  echo   doalltests delphiVersion
  echo.
  echo Examples:
  echo   doalltests 2007
  echo   doalltests XE5

:exit