#!/usr/bin/env bash

# Colours for logging messages
red='\033[0;31m'
green='\033[0;32m'
orange='\033[0;33m'
cyan='\033[0;36m'
noColour='\033[0m'

# param $1: info message
log_info(){
    if [[ -f $LOG ]]; then
        echo -e "${cyan}"`date`" ${green}[ INFO ] $1 ${noColour}" | tee -a $LOG
    else
        echo -e "${cyan}"`date`" ${green}[ INFO ] $1 ${noColour}"
    fi
}

# param $1: warning message
log_warn(){
    if [[ -f $LOG ]]; then
        echo -e "${cyan}"`date`" ${orange}[ WARN ] $1 ${noColour}" | tee -a $LOG
    else
        echo -e "${cyan}"`date`" ${orange}[ WARN ] $1 ${noColour}"
    fi
}

# param $1: error message
log_error(){
    if [[ -f $LOG ]]; then
        echo -e "${cyan}"`date`" ${red}[ ERROR ] $1 ${noColour}" | tee -a $LOG
    else
        echo -e "${cyan}"`date`" ${red}[ ERROR ] $1 ${noColour}"
    fi
}

# param $1: error message (default is "An error occured, exiting...")
# param $2: error code (default is 1)
exit_error(){
  if [[ $? -ne 0 ]]; then
      log_error ${1:-"An error occured, exiting..."}
      exit ${2:-1}
  fi
}

