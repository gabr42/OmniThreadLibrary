@echo off
  if %1.==. goto usage

echo.
echo %1

title OmniThreadLibrary %1

echo.
echo Unit tests
echo.
  pushd unittests
  call buildandrun
  if errorlevel 1 goto exit
  popd

echo.
echo Tests
echo.
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