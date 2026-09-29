@echo off
setlocal
cd /d "%~dp0"

echo ============================================================
echo FABDEM Watershed Runtime
echo Migracion Git LFS a GitHub Releases
echo ============================================================
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
  echo.
  echo R debe estar instalado normalmente en una de estas ubicaciones:
  echo   C:\Program Files\R\R-x.x.x\bin\Rscript.exe
  echo   %%LOCALAPPDATA%%\Programs\R\R-x.x.x\bin\Rscript.exe
  echo.
  pause
  exit /b 1
)

echo Rscript encontrado:
echo   %RSCRIPT%
echo.

"%RSCRIPT%" "10_migrar_lfs_a_releases.R"

if errorlevel 1 (
  echo.
  echo ERROR: la migracion no termino correctamente.
  pause
  exit /b 1
)

echo.
echo Migracion completada.
pause
