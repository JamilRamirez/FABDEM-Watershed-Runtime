@echo off
setlocal
cd /d "%~dp0"

set "GIT=C:\Program Files\Git\cmd\git.exe"
if not exist "%GIT%" (
  echo ERROR: No se encontro Git en:
  echo %GIT%
  goto :fail
)

set "REPO_URL=https://github.com/JamilRamirez/FABDEM-Watershed-Runtime.git"
if not "%~1"=="" set "REPO_URL=%~1"
echo Repositorio de destino: %REPO_URL%

powershell.exe -NoProfile -Command "$bad = Get-ChildItem -LiteralPath . -Recurse -Force -File ^| Where-Object { $_.Length -gt 100MB -and $_.FullName -notmatch '[\\/].git[\\/]' }; if ($bad) { $bad ^| ForEach-Object { Write-Host ('SUPERA 100 MiB: ' + $_.FullName) }; exit 1 }"
if errorlevel 1 goto :fail

powershell.exe -NoProfile -Command "$size = (Get-ChildItem -LiteralPath . -Recurse -Force -File ^| Where-Object { $_.FullName -notmatch '[\\/].git[\\/]' } ^| Measure-Object Length -Sum).Sum; Write-Host ('Tamano aproximado de Runtime: {0:N2} GiB' -f ($size / 1GB))"
echo Los archivos TIF, GPKG y RDS se almacenaran mediante Git LFS.
choice /C SN /N /M "Continuar con la preparacion y subida? [S/N]: "
if errorlevel 2 exit /b 0

if not exist ".git" (
  "%GIT%" init || goto :fail
)
"%GIT%" lfs install --local || goto :fail
"%GIT%" branch -M main || goto :fail
"%GIT%" add . || goto :fail
"%GIT%" diff --cached --quiet
if errorlevel 1 (
  "%GIT%" commit -m "Initial commit" || goto :fail
) else (
  echo No hay cambios nuevos para crear un commit.
)

"%GIT%" remote get-url origin >nul 2>&1
if errorlevel 1 (
  "%GIT%" remote add origin "%REPO_URL%" || goto :fail
) else (
  "%GIT%" remote set-url origin "%REPO_URL%" || goto :fail
)

"%GIT%" push -u origin main || goto :fail
echo.
echo Runtime se subio correctamente.
pause
exit /b 0

:fail
echo.
echo La preparacion o subida no se completo. Revisa el mensaje anterior.
pause
exit /b 1
