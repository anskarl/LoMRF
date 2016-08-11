@echo off
setlocal enabledelayedexpansion
:: Get the bin path
SET bin_dir=%~dp0

:: Get the LoMRF paths
pushd "%bin_dir%"
cd ..
SET base_dir=%cd%
SET lib_dir=%base_dir%\lib
SET etc_dir=%base_dir%\etc
popd


call %bin_dir%\inc.env.bat


java %VM_ARGS% -Djava.ext.dirs=%lib_dir% lomrf.app.LoMRF %*