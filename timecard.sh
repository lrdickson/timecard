#! /usr/bin/env sh

if type nu > /dev/null; then
  nu -c "timecard-json $@ | from json | table -e"
else
  timecard-json $@
fi
