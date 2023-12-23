import std/algorithm
import std/os
import std/osproc
import std/strformat
import std/strutils
import std/threadpool
from program import Program

type NixApps* = object
  thread: FlowVar[seq[Program]]
  results: seq[Program]

const nixLocate = getEnv("NIX_LOCATE_BIN")

proc parseNixLocateLine(runDesktopFileBin: string, line: string): Program =
  let columns = splitWhitespace(line)
  var appName = columns[0]
  removeSuffix(appName, ".out")
  # The desktop file path will be something like:
  #     /nix/store/123-my-app/some/path/to/app.desktop
  # We'd like to keep this bit:
  #     some/path/to/app.desktop
  let desktopFile = tailDir(tailDir(tailDir(tailDir(columns[3]))))
  Program(
    name: appName,
    searchName: toLower(appName),
    runCmd: &"systemd-run --user {runDesktopFileBin} {appName} {desktopFile}",
  )

proc findAll(): seq[Program] =
  let prog = startProcess(
      nixLocate,
      "",
      ["--top-level", "--regex", "^/share/applications/.*\\.desktop$"],
    )
  let selfBin = getAppFilename()
  var applications: seq[Program] = @[]
  for line in lines(prog):
    let app = parseNixLocateLine(selfBin, line)
    add(applications, app)
  applications.sortedByIt(-len(it.name))

proc fetch*(): NixApps =
  NixApps(
    thread: spawn findAll(),
    results: @[],
  )

proc list*(nixApps: var NixApps): seq[Program] =
  if len(nixApps.results) == 0:
    nixApps.results = ^nixApps.thread
  nixApps.results
