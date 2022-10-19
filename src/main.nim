import std/exitprocs
import std/locks
import std/os
import std/sequtils
import std/streams
import std/strformat
import std/strscans
import std/strutils
import std/terminal
import std/threadpool

const ETX = '\3'
const EOT = '\4'
const ESC = '\27'
const DEL = '\127'
const NAK = '\21'
const CR = '\13'

proc updateTyped(typed: var string, char: char): void =
  case char
    of NAK:
      typed = ""
    of DEL:
      if len(typed) > 0:
        typed = typed[0..^2]
    of Letters:
      add(typed, char)
    else:
      discard

# The main thread listens for keyboard input and updates the prompt immediately.
proc readline(onChange: var Channel[char], stdoutLock: var Lock): bool =
  var typed = ""
  while true:
    let char = getch()
    send(onChange, char)

    case char
      of ETX, EOT, ESC:
        return false
      of CR:
        return true
      else:
        updateTyped(typed, char)

    withLock(stdoutLock):
      eraseLine()
      write(stdout, typed)

type DesktopApp* = object
  name*: string
  exec*: string

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

proc parseDesktopFile(path: string): DesktopApp =
  var stream = newFileStream(path)
  defer: stream.close()
  var name = path
  var exec = "false"
  while not atEnd(stream):
    let line = readLine(stream)
    var key, val: string
    if scanf(line, "$+=$+", key, val):
      case key
        of "Name":
          name = val
        of "Exec":
          exec = cleanupExec(val)
        else:
          discard
  return DesktopApp(name: name, exec: exec)

proc findDesktopApps(): seq[DesktopApp] =
  let xdgDataDirs = getEnv("XDG_DATA_DIRS").split(":")
  var applications: seq[DesktopApp] = @[]
  for dir in xdgDataDirs:
    for file in walkFiles(fmt"{dir}/applications/*.desktop"):
      let app = parseDesktopFile(file)
      add(applications, app)
  return deduplicate(applications)

# Calculate what options to show, on a separate thread so we don't block UI.
proc showOptions(onChange: ptr Channel[char],
    stdoutLock: ptr Lock): string {.thread.} =
  var typed = ""
  let options = findDesktopApps()
  while true:
    let char = recv(onChange[])
    case char
      of CR:
        return options[0].exec # Todo let the user select the option
      else:
        updateTyped(typed, char)

    withLock(stdoutLock[]):
      eraseScreen()
      for option in options:
        write(stdout, &"\r{option.name}\r\n")
      write(stdout, typed)
      flushFile(stdout)

proc main(): void =
  var onChange: Channel[char]
  open(onChange)

  var stdoutLock: Lock
  initLock(stdoutLock)

  addExitProc(resetAttributes)

  let thread = spawn showOptions(addr(onChange), addr(stdoutLock))

  if readline(onChange, stdoutLock):
    eraseScreen()
    echo &"\r{^thread}" # TODO launch a program here

main()
