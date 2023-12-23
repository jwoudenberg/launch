# Description of a program a user might select to run.
type Program* = object
  runCmd*: string
  name*: string
  searchName*: string
  background*: bool
