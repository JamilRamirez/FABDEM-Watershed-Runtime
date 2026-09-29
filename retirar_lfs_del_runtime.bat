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

where Rscript >nul 2>&1
if errorlevel 1 (
  echo ERROR: Rscript no esta disponible en PATH.
  pause
  exit /b 1
)

Rscript "11_retirar_lfs_del_runtime.R"

if errorlevel 1 (
  echo.
  echo ERROR: LFS NO fue retirado completamente.
  pause
  exit /b 1
)

echo.
echo Git LFS retirado del branch main.
pause
