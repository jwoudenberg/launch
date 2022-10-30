import std/os
import std/json
import std/strformat
import std/strutils
import std/sequtils
from program import Program

proc parseEmoji(json: string): seq[Program] =
  proc parseOne(node: JsonNode): Program =
    let description = getStr(node["description"])
    let emoji = getStr(node["emoji"])
    let wtype = getEnv("WTYPE_BIN")
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

const all*: seq[Program] = parseEmoji(staticRead("./data/emoji.json"))
