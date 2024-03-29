import std/algorithm
import std/os
import std/options
import std/strformat
import std/strutils
import std/threadpool
from program import Program

type Passwords* = object
  thread: FlowVar[seq[Program]]
  results: Option[seq[Program]]

proc fromPassPath(line: string): Program =
  var path = line
  removeSuffix(path, ".age")
  removeSuffix(path, ".gpg")
  const wtype = getEnv("WTYPE_BIN")
  if wtype == "":
    var e: ref OSError
    new(e)
    e.msg = "WTYPE_BIN variable not set"
    raise e
  Program(
    name: path,
    searchName: path,
    runCmd: &"sh -c 'pass show {path} | head -n 1 | systemd-run --user --pipe {wtype} -s 100 - >/dev/null 2>&1'",
  )

proc findAll(): seq[Program] =
  let passDir = getEnv("PASSAGE_DIR", default = getEnv("PASSWORD_STORE_DIR",
      default = "~/.password-store"))
  var applications: seq[Program] = @[]
  for path in os.walkDirRec(expandTilde(passDir), relative = true):
    let app = fromPassPath(path)
    add(applications, app)
  applications.sortedByIt(-len(it.name))

proc fetch*(): Passwords =
  Passwords(
    thread: spawn findAll(),
  )

proc list*(nixApps: var Passwords): seq[Program] =
  if nixApps.results.isNone:
    nixApps.results = some(^nixApps.thread)
  nixApps.results.get()
