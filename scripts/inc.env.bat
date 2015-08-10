:: JVM Parameters
set "VM_ARGS=  "

::
:: Logging configuration
::
:: To enable debug information, export LOMRF_DEBUG=1 for storing further 
:: debug information in 'debug.log' file, otherwise use default (INFO level)
:: console output logging.
::


if not defined LOMRF_DEBUG (
  set "LOMRF_DEBUG=0" 
)



if %LOMRF_DEBUG% equ 1 (

  if not exist %etc_dir%\logback-debug.xml (
    echo Cannot find logging configuration file '%etc_dir%\logback-debug.xml'
    exit /b 1
  )

  echo Debug output is enabled 'LOMRF_DEBUG=1'
  echo Debug output will be stored into the 'debug.log' file

  SET "VM_ARGS=%VM_ARGS% -Dlogback.configurationFile=%etc_dir%\logback-debug.xml "

) else (

  if not exist %etc_dir%\logback.xml (
    exit /b 1
  ) else (
    SET "VM_ARGS=%VM_ARGS% -Dlogback.configurationFile=%etc_dir%\logback.xml "
  )

)

