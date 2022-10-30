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

proc parseNixLocateLine(openDesktopFileBin: string, line: string): Program =
  let columns = splitWhitespace(line)
  var appName = columns[0]
  removeSuffix(appName, ".out")
  let desktopFile = columns[3]
  Program(
    name: appName,
    searchName: toLower(appName),
    runCmd: &"nix shell nixpkgs#{appName} --command {openDesktopFileBin} {desktopFile}",
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
  applications

proc fetch*(): NixApps =
  NixApps(
    thread: spawn findAll(),
    results: @[],
  )

proc list*(nixApps: var NixApps): seq[Program] =
  if len(nixApps.results) == 0:
    nixApps.results = ^nixApps.thread
  nixApps.results
