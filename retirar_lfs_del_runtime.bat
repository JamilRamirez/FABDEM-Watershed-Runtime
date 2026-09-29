@echo off
setlocal
cd /d "%~dp0"

echo ============================================================
echo FABDEM Watershed Runtime
echo FASE 2 - Retirar Git LFS del branch main
echo ============================================================
echo.
echo EJECUTAR SOLO DESPUES DE VERIFICAR LA WEB CON RELEASES.
echo.

set "RSCRIPT="

for /f "delims=" %%I in ('where Rscript 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%I"
)

if not defined RSCRIPT (
  for /f "usebackq delims=" %%I in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$roots=@((Join-Path $env:ProgramFiles 'R'),(Join-Path $env:LOCALAPPDATA 'Programs\R')); if(${env:ProgramFiles(x86)}){$roots += (Join-Path ${env:ProgramFiles(x86)} 'R')}; $c=@(); foreach($r in $roots){if(Test-Path $r){$c += Get-ChildItem -Path $r -Directory -ErrorAction SilentlyContinue | ForEach-Object { Join-Path $_.FullName 'bin\Rscript.exe' } | Where-Object { Test-Path $_ }}}; $p=$c | Sort-Object -Descending | Select-Object -First 1; if($p){[Console]::Write($p)}"`) do (
    set "RSCRIPT=%%I"
  )
)

if not defined RSCRIPT (
  echo ERROR: no se encontro Rscript.exe.
  pause
  exit /b 1
)

echo Rscript encontrado:
echo   %RSCRIPT%
echo.

"%RSCRIPT%" "11_retirar_lfs_del_runtime.R"

if errorlevel 1 (
  echo.
  echo ERROR: LFS NO fue retirado completamente.
  pause
  exit /b 1
)

echo.
echo Git LFS retirado del branch main.
pause
