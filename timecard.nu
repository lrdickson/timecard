#!/usr/bin/env nu
def main [...x] {
  src/timecard/bin/Release/net8.0/linux-x64/timecard ...$x | from json
}
