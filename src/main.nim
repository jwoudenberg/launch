import std/locks
import std/strformat
import std/terminal
import std/threadpool
import std/exitprocs
import strutils

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

# Calculate what options to show, on a separate thread so we don't block UI.
proc showOptions(onChange: ptr Channel[char],
    stdoutLock: ptr Lock): string {.thread.} =
  var typed = ""
  let options = ["a hat", "a stick", "an umbrella"]
  while true:
    let char = recv(onChange[])
    case char
      of CR:
        return options[0]
      else:
        updateTyped(typed, char)

    withLock(stdoutLock[]):
      eraseScreen()
      for option in options:
        write(stdout, &"\r{option}\r\n")
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
