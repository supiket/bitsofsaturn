#!/bin/bash

LOG_FILE="$HOME/startup-output.log"
ERROR_FILE="$HOME/startup-error.log"
PROJECT_DIR="$HOME/personal/bitsofsaturn"
DATE_FORMAT=$(date '+%Y-%m-%d %H:%M:%S')

echo "$DATE_FORMAT - bits of saturn is running" >> $LOG_FILE

if [ ! -d "$PROJECT_DIR" ]; then
    echo "$DATE_FORMAT - ERROR: project directory $PROJECT_DIR not found" >> $ERROR_FILE
    exit 1
fi

cd "$PROJECT_DIR" || {
    echo "$DATE_FORMAT - ERROR: failed to cd into $PROJECT_DIR" >> $ERROR_FILE
    exit 1
}

if [ ! -x "/run/current-system/sw/bin/nix" ]; then
    echo "$DATE_FORMAT - ERROR: nix not found or not executable" >> $ERROR_FILE
    exit 1
fi

echo "$DATE_FORMAT - running nix develop..." >> $LOG_FILE
echo "$DATE_FORMAT - in nix environment" >> $LOG_FILE
/run/current-system/sw/bin/nix develop --command cabal run >> $LOG_FILE 2>> $ERROR_FILE
