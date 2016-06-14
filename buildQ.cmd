@echo off
cls

REM .paket\paket.bootstrapper.exe
REM if errorlevel 1 (
REM   exit /b %errorlevel%
REM )

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
