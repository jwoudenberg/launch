import std/algorithm
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
    background: true,
  )


proc find*(): seq[Program] =
  let xdgDataDirs = getEnv("XDG_DATA_DIRS").split(":")
  var applications: seq[Program] = @[]
  for dir in xdgDataDirs:
    for file in walkFiles(fmt"{dir}/applications/*.desktop"):
      let app = parseDesktopFile(file)
      add(applications, app)
  deduplicate(applications).sortedByIt(-len(it.name))

# This is called indirectly from the nixapps.nim module. We know the name of a
# nix package and the relative location of the desktop file within the package,
# but the exact nix hash we can't trust because it'll come from a nix-index
# cache that might be a bit stale.
proc run*(nixPackage: string, desktopFile: string) =
  let runtimeDir = getEnv("XDG_RUNTIME_DIR", "/tmp")
  let outDir = joinPath(runtimeDir, &"jlaunch")
  discard execCmd(&"nix build --out-link '{outDir}' 'nixpkgs#{nixPackage}'")
  let path = joinPath(outDir, desktopFile)
  let program = parseDesktopFile(path)
  discard execCmd(&"systemd-run --user nix shell nixpkgs#{nixPackage} --command {program.runCmd}")
