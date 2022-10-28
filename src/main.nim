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

const ETX = '\3'
const EOT = '\4'
const ESC = '\27'
const DEL = '\127'
const NAK = '\21' # Ctrl+U
const CR = '\13'
const SI = '\14' # Ctrl+N
const DLE = '\16' # Ctrl+P

type Option = object
  selectionCmd: string
  displayText: string
  searchText: string

type IndexedOption = object
  option: ref Option
  searchIndex: int

type SearchFrame = ref object
  typed: string
  previous: SearchFrame
  options: seq[IndexedOption]

proc initFrame(options: seq[Option]): SearchFrame =
  proc initWithIndex(option: Option): IndexedOption =
    var optionRef: ref Option = new(Option)
    optionRef[] = option
    IndexedOption(option: optionRef, searchIndex: 0)

  SearchFrame(
    typed: "",
    previous: nil,
    options: map(options, initWithIndex),
  )

proc popFrame(frame: SearchFrame): SearchFrame =
  if isNil(frame.previous):
    frame
  else:
    frame.previous

proc pushFrame(frame: SearchFrame, char: char): SearchFrame =
  var options: seq[IndexedOption] = @[]

  for old in frame.options:
    let hit = find(old.option.searchText, toLowerAscii(char), old.searchIndex)
    if hit >= 0:
      let new = IndexedOption(
        option: old.option,
        searchIndex: hit + 1,
      )
      add(options, new)

  SearchFrame(
    typed: &"{frame.typed}{char}",
    previous: frame,
    options: options,
  )

proc resetFrame(frame: SearchFrame): SearchFrame =
  if isNil(frame.previous):
    frame
  else:
    resetFrame(frame.previous)

proc updateFrame(frame: SearchFrame, char: char): SearchFrame =
  case char
  of NAK:
    resetFrame(frame)
  of DEL:
    popFrame(frame)
  else:
    pushFrame(frame, char)

proc writePrompt(text: string) =
  eraseLine()
  write(stdout, "> ")
  write(stdout, text)
  flushFile(stdout)

# The main thread listens for keyboard input and updates the prompt immediately.
proc readline(onChange: var Channel[char], stdoutLock: var Lock): bool =
  var frame = initFrame(@[])
  while true:
    let char = getch()
    send(onChange, char)

    case char
      of ETX, EOT, ESC:
        return false
      of CR:
        return true
      else:
        frame = updateFrame(frame, char)

    withLock(stdoutLock):
      writePrompt(frame.typed)

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

proc parseDesktopFile(path: string): Option =
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
  Option(
    displayText: name,
    searchText: toLower(name),
    selectionCmd: exec,
  )

proc findDesktopApps(): seq[Option] =
  let xdgDataDirs = getEnv("XDG_DATA_DIRS").split(":")
  var applications: seq[Option] = @[]
  for dir in xdgDataDirs:
    for file in walkFiles(fmt"{dir}/applications/*.desktop"):
      let app = parseDesktopFile(file)
      add(applications, app)
  deduplicate(applications)

proc parseEmoji(json: string): seq[Option] =
  proc parseOne(node: JsonNode): Option =
    let description = getStr(node["description"])
    let emoji = getStr(node["emoji"])
    Option(
      selectionCmd: &"echo {emoji}",
      displayText: &"{emoji} {description}",
      searchText: toLower(description),
    )

  getElems(parseJson(json)).map(parseOne)

const emoji: seq[Option] = parseEmoji(staticRead("./data/emoji.json"))

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
proc showOptions(onChange: ptr Channel[char],
    stdoutLock: ptr Lock): string {.thread.} =
  var selectedOption = 0
  var frame = initFrame(findDesktopApps())
  while true:
    withLock(stdoutLock[]):
      let selectionIndex = len(frame.options) - selectedOption - 1
      eraseScreen()
      for (index, indexedOption) in mpairs(frame.options):
        let line = &"\r{indexedOption.option.displayText}\r\n"
        if index == selectionIndex:
          styledWrite(stdout, styleReverse, line)
        else:
          write(stdout, line)
      writePrompt(frame.typed)

    for char in atLeastOne(onChange[]):
      case char
        of CR:
          let selectionIndex = len(frame.options) - selectedOption - 1
          return frame.options[selectionIndex].option.selectionCmd
        of SI:
          selectedOption -= 1
        of DLE:
          selectedOption += 1
        else:
          selectedOption = 0
          frame = updateFrame(frame, char)

      selectedOption = clamp(selectedOption, 0, len(frame.options) - 1)


proc main(): void =
  var onChange: Channel[char]
  open(onChange)

  var stdoutLock: Lock
  initLock(stdoutLock)

  addExitProc(resetAttributes)

  let thread = spawn showOptions(addr(onChange), addr(stdoutLock))

  if readline(onChange, stdoutLock):
    eraseScreen()
    discard execCmd(&"systemd-run --user {^thread}")

main()
