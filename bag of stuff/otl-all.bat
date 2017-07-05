@echo off

  if not %2.==. goto buildone
  if %1.==. goto buildall
  
  if /i %1==2007    call :build 5.0 2007
  if /i %1==2009    call :build 6.0 2009
  if /i %1==2010    call :build 7.0 2010
  if /i %1==XE      call :build 8.0 XE
  if /i %1==XE2     call :build 9.0 XE2
  if /i %1==XE3     call :build 10.0 XE3
  if /i %1==XE4     call :build 11.0 XE4
  if /i %1==XE5     call :build 12.0 XE5
  if /i %1==XE6     call :build 14.0 XE6
  if /i %1==XE7     call :build 15.0 XE7
  if /i %1==XE8     call :build 16.0 XE8
  if /i %1==10      call :build 17.0 10Seattle
  if /i %1==Seattle call :build 17.0 10Seattle
  if /i %1==10.1    call :build 18.0 10_1Berlin
  if /i %1==Berlin  call :build 18.0 10_1Berlin
  if /i %1==10.2    call :build 19.0 10_2Tokyo
  if /i %1==Tokyo   call :build 19.0 10_2Tokyo
  goto exit

:buildall
  call :build 5.0 2007
  call :build 6.0 2009
  call :build 7.0 2010
  call :build 8.0 XE
  call :build 9.0 XE2
  call :build 10.0 XE3
  call :build 11.0 XE4
  call :build 12.0 XE5
  call :build 14.0 XE6
  call :build 15.0 XE7
  call :build 16.0 XE8
  call :build 17.0 10Seattle
  call :build 18.0 10_1Berlin
  call :build 19.0 10_2Tokyo

  goto exit

:buildone
  call :build %1 %2
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
