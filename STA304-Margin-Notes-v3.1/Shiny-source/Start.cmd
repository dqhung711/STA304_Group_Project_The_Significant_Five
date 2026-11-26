@echo off
setlocal EnableExtensions DisableDelayedExpansion
pushd "%~dp0"
if errorlevel 1 goto :extract
if not exist "app\app.R" goto :extract

rem Use R on PATH or its registered installation. Nothing is installed or downloaded.
set "R_BIN="
for %%R in (Rscript.exe) do set "R_BIN=%%~$PATH:R"
if defined R_BIN goto :check
for %%V in (64 32) do (
  for %%H in (HKCU HKLM) do (
    for /f "tokens=2,*" %%A in ('reg.exe query "%%H\SOFTWARE\R-core\R" /v InstallPath /reg:%%V 2^>nul') do (
      if /i "%%A"=="REG_SZ" if exist "%%B\bin\Rscript.exe" (
        set "R_BIN=%%B\bin\Rscript.exe"
        goto :check
      )
    )
  )
)
goto :offline

:check
"%R_BIN%" --vanilla "tools\launch.R" check
if errorlevel 1 goto :offline
echo Opening the local R Shiny website. Keep this window open while using it.
"%R_BIN%" --vanilla "tools\launch.R" run
if errorlevel 1 goto :offline
popd
exit /b 0

:offline
echo Opening the offline interactive website in your default browser.
start "" "%~dp0..\OPEN WEBSITE.html"
if errorlevel 1 goto :manual
popd
exit /b 0

:manual
echo Open OPEN WEBSITE.html in the parent folder in Edge, Chrome, or Firefox.
pause
popd
exit /b 1

:extract
echo Extract the entire ZIP first, then double-click Start.cmd in the extracted folder.
pause
exit /b 1
