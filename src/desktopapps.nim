import std/os
import std/osproc
import std/sequtils
import std/streams
import std/strformat
import std/strscans
import std/strutils
from program import Program

proc cleanupExec(cmd: string): string =
  multiReplace(
    cmd,
    ("%f", ""),
    ("%F", ""),
    ("%u", ""),
    ("%U", ""),
    ("%d", ""),
    ("%D", ""),
    ("%n", ""),
    ("%N", ""),
    ("%i", ""),
    ("%c", ""),
    ("%k", ""),
    ("%v", ""),
    ("%m", ""),
  )

proc parseDesktopFile(path: string): Program =
  var stream = newFileStream(path)
  defer: stream.close()
  var name = path
  var exec = "false"
  var inDesktopEntry = false
  while not atEnd(stream):
    let line = readLine(stream)
    var key, val: string
    if line == "[Desktop Entry]":
      inDesktopEntry = true
      continue
    if len(line) > 0 and line[0] == '[':
      inDesktopEntry = false
      continue
    if not inDesktopEntry:
      continue
    if scanf(line, "$+=$+", key, val):
      case key
        of "Name":
          name = val
        of "Exec":
          exec = cleanupExec(val)
        else:
          discard
  Program(
    name: name,
    searchName: toLower(name),
    runCmd: exec,
  )


proc find*(): seq[Program] =
  let xdgDataDirs = getEnv("XDG_DATA_DIRS").split(":")
  var applications: seq[Program] = @[]
  for dir in xdgDataDirs:
    for file in walkFiles(fmt"{dir}/applications/*.desktop"):
      let app = parseDesktopFile(file)
      add(applications, app)
  deduplicate(applications)

proc open*(path: string) =
  let program = parseDesktopFile(path)
  discard execCmd(program.runCmd)
