@echo off
setlocal
cd /d "%~dp0"

echo ============================================================
echo FABDEM Watershed Runtime
echo Migracion Git LFS a GitHub Releases
echo ============================================================
echo.

where Rscript >nul 2>&1
if errorlevel 1 (
  echo ERROR: Rscript no esta disponible en PATH.
  pause
  exit /b 1
)

Rscript "10_migrar_lfs_a_releases.R"

if errorlevel 1 (
  echo.
  echo ERROR: la migracion no termino correctamente.
  pause
  exit /b 1
)

echo.
echo Migracion completada.
pause
