import std/exitprocs
import std/locks
import std/os
import std/osproc
import std/sequtils
import std/strformat
import std/strutils
import std/terminal
import std/threadpool
from program import Program
from emoji import nil
from nixapps import NixApps
from desktopapps import nil

const ETX = '\3' # Ctrl+C
const EOT = '\4' # Ctrl+D
const ESC = '\27' # Escape
const DEL = '\127' # Backspace
const NAK = '\21' # Ctrl+U
const CR = '\13' # Enter
const SI = '\14' # Ctrl+N
const DLE = '\16' # Ctrl+P

# A program and an index used for fuzzy matching a search string. The index
# represents the index in the program's 'searchName' we should start searching
# from when the user types an additional character.
type IndexedProgram = object
  program: ref Program
  searchIndex: int

# A sequence of options matching a given fuzzy search string.
type SearchFrame = object
  options: seq[IndexedProgram]

# The state of a fuzzy search operation, containing one or more search frames.
# Typing a new character adds a frame, presssing backspace removes one.
type SearchState = object
  typed: string
  selectedProgram: int
  nixApps: NixApps
  frameHead: SearchFrame
  frameTail: seq[SearchFrame]

# Displaying too many options is pointless because they won't fit on the screen,
# and rendering all of them will take a while causing flickering.
const MAX_DISPLAY_OPTIONS = 20

proc updateTyped(typed: var string, char: char) =
  case char:
  of NAK:
    typed = ""
  of DEL:
    if len(typed) > 0:
      typed = typed[0..^2]
  of CR, SI, DLE:
    discard
  else:
    add(typed, char)

proc toIndexed(program: Program): IndexedProgram =
  var programRef: ref Program = new(Program)
  programRef[] = program
  IndexedProgram(program: programRef, searchIndex: 0)

proc resetIndex(program: IndexedProgram): IndexedProgram =
  IndexedProgram(
    program: program.program,
    searchIndex: 0,
  )

proc nextFrame(frame: SearchFrame, char: char): SearchFrame =
  var options: seq[IndexedProgram] = @[]

  for old in frame.options:
    let hit = find(old.program.searchName, toLowerAscii(char), old.searchIndex)
    if hit >= 0:
      let new = IndexedProgram(
        program: old.program,
        searchIndex: hit + 1,
      )
      add(options, new)

  SearchFrame(options: options)

proc lastFrame(state: var SearchState): SearchFrame =
  let lenTail = len(state.frameTail)
  if lenTail > 0:
    state.frameTail[lenTail - 1]
  else:
    state.frameHead

# The amount of options that should be currently displayed.
proc displayLen(state: var SearchState): int =
  let frame = lastFrame(state)
  min(MAX_DISPLAY_OPTIONS, len(frame.options))

proc getSelectionIndex(state: var SearchState): int =
  displayLen(state) - state.selectedProgram - 1


proc lastOptions(state: var SearchState): seq[IndexedProgram] =
  lastFrame(state).options[^displayLen(state) .. ^1]

proc updateState(state: var SearchState, char: char): ref Program =
  updateTyped(state.typed, char)
  if (len(state.frameTail) == 0 and char == ':'):
    let emojiFrame = SearchFrame(options: map(emoji.all, toIndexed))
    add(state.frameTail, emojiFrame)
  elif (len(state.frameTail) == 0 and char == ','):
    let options = map(nixapps.list(state.nixApps), toIndexed)
    add(state.frameTail, SearchFrame(options: options))
  else:
    case char
    of NAK:
      state.frameTail = @[]
    of DEL:
      if len(state.frameTail) > 0:
        discard pop(state.frameTail)
    of SI:
      state.selectedProgram = clamp(
        state.selectedProgram - 1,
        0,
        displayLen(state) - 1
      )
    of DLE:
      state.selectedProgram = clamp(
        state.selectedProgram + 1,
        0,
        displayLen(state) - 1
      )
    of CR:
      let options = lastOptions(state)
      let selectionIndex = getSelectionIndex(state)
      if selectionIndex >= 0:
        return options[selectionIndex].program
    of ' ':
      let options = map(lastFrame(state).options, resetIndex)
      let next = SearchFrame(options: options)
      add(state.frameTail, next)
    else:
      state.selectedProgram = 0
      let next = nextFrame(lastFrame(state), char)
      add(state.frameTail, next)

proc writePrompt(text: string) =
  eraseLine()
  write(stdout, "> ")
  write(stdout, text)
  flushFile(stdout)

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
      writePrompt(typed)

# Block on reading message from channel, then continue reading until empty.
iterator atLeastOne[T](channel: var Channel[T]): T =
  yield recv(channel)
  while true:
    let (success, msg) = tryRecv(channel)
    if success:
      yield msg
    else:
      break

# Calculate what options to show, on a separate thread so we don't block UI.
proc showPrograms(onChange: ptr Channel[char],
    stdoutLock: ptr Lock): ref Program {.thread.} =
  let frameHead = SearchFrame(options: map(desktopapps.find(), toIndexed))
  var state = SearchState(
    typed: "",
    selectedProgram: 0,
    nixApps: nixapps.fetch(),
    frameHead: frameHead,
    frameTail: @[],
  )
  while true:
    let selectionIndex = getSelectionIndex(state)

    withLock(stdoutLock[]):
      eraseScreen()
      var options = lastOptions(state)
      for (index, indexedProgram) in mpairs(options):
        let line = &"\r{indexedProgram.program.name}\r\n"
        if index == selectionIndex:
          styledWrite(stdout, styleReverse, line)
        else:
          write(stdout, line)
      writePrompt(state.typed)

    for char in atLeastOne(onChange[]):
      let program = updateState(state, char)
      if program != nil:
        return program

proc main(): void =
  # If we're called with a parameter, assume we're passed a .desktop file.
  let params = commandLineParams()
  if len(params) > 0:
    desktopapps.run(params[0], params[1])
    return

  var onChange: Channel[char]
  open(onChange)

  var stdoutLock: Lock
  initLock(stdoutLock)

  addExitProc(resetAttributes)

  let thread = spawn showPrograms(addr(onChange), addr(stdoutLock))

  if readline(onChange, stdoutLock):
    eraseScreen()
    let program = ^thread
    discard execCmd(&"systemd-run --user {program.runCmd}")

main()
