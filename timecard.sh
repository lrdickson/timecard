#! /usr/bin/env sh

# echo Provided arguments: $@
if type nu > /dev/null; then
  nucommand="timecard-json f hours.csv $@ | from json | table -e"
  # echo $nucommand
  nu -c "$nucommand"
else
  timecard-json f /mnt/hgfs/c/Users/U215393/Documents/hours.csv "$@"
fi
