import std/exitprocs
import std/json
import std/locks
import std/os
import std/osproc
import std/sequtils
import std/streams
import std/strformat
import std/strscans
import std/strutils
import std/terminal
import std/threadpool

const ETX = '\3' # Ctrl+C
const EOT = '\4' # Ctrl+D
const ESC = '\27' # Escape
const DEL = '\127' # Backspace
const NAK = '\21' # Ctrl+U
const CR = '\13' # Enter
const SI = '\14' # Ctrl+N
const DLE = '\16' # Ctrl+P

# Description of a program a user might select to run.
type Program = object
  runCmd: string
  name: string
  searchName: string

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
  frameHead: SearchFrame
  frameTail: seq[SearchFrame]

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

proc parseEmoji(json: string): seq[Program] =
  proc parseOne(node: JsonNode): Program =
    let description = getStr(node["description"])
    let emoji = getStr(node["emoji"])
    let wtype = os.getEnv("WTYPE_BIN")
    if wtype == "":
      var e: ref OSError
      new(e)
      e.msg = "WTYPE_BIN variable not set"
      raise e
    Program(
      runCmd: &"{wtype} -s 100 '{emoji}'",
      name: &"{emoji} {description}",
      searchName: toLower(description),
    )
  getElems(parseJson(json)).map(parseOne)

const emoji: seq[Program] = parseEmoji(staticRead("./data/emoji.json"))

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

proc getSelectionIndex(state: var SearchState): int =
  let options = lastFrame(state).options
  len(options) - state.selectedProgram - 1

proc updateState(state: var SearchState, char: char): ref Program =
  updateTyped(state.typed, char)
  if (len(state.frameTail) == 0 and char == ':'):
    let emojiFrame = SearchFrame(options: map(emoji, toIndexed))
    add(state.frameTail, emojiFrame)
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
        len(lastFrame(state).options) - 1
      )
    of DLE:
      state.selectedProgram = clamp(
        state.selectedProgram + 1,
        0,
        len(lastFrame(state).options) - 1
      )
    of CR:
      let options = lastFrame(state).options
      let selectionIndex = getSelectionIndex(state)
      if selectionIndex >= 0:
        return options[selectionIndex].program
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

proc findDesktopApps(): seq[Program] =
  let xdgDataDirs = getEnv("XDG_DATA_DIRS").split(":")
  var applications: seq[Program] = @[]
  for dir in xdgDataDirs:
    for file in walkFiles(fmt"{dir}/applications/*.desktop"):
      let app = parseDesktopFile(file)
      add(applications, app)
  deduplicate(applications)

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
  let frameHead = SearchFrame(options: map(findDesktopApps(), toIndexed))
  var state = SearchState(
    typed: "",
    frameHead: frameHead,
    frameTail: @[],
  )
  while true:
    let selectionIndex = getSelectionIndex(state)
    let frame = lastFrame(state)
    var lastOptions = frame.options[0..(min(20, len(frame.options)) - 1)]

    withLock(stdoutLock[]):
      eraseScreen()
      for (index, indexedProgram) in mpairs(lastOptions):
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
