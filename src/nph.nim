#           nph
#        (c) Copyright 2023 Jacek Sieka
## Opinionated source code formatter

import
  "."/[
    astcmp, astyaml, phast, phastyaml, phmsgs, phlineinfos, phoptions, phparser,
    phrenderer
  ]

import "$nim"/compiler/idents

import std/[parseopt, strutils, os, sequtils]

static:
  doAssert (NimMajor, NimMinor, NimPatch) == (2, 0, 0),
    "nph needs to be compiled with nim 2.0.0 exactly for now"

const
  Version = gorge("git describe --long --dirty --always --tags")
  Usage =
    "nph - Nim formatter " & Version &
    """
Usage:
  nph [options] nimfiles...
Options:
  --check               check the formatting instead of performing it
  --out:file            set the output file (default: overwrite the input file)
  --outDir:dir          set the output dir (default: overwrite the input files)
  --version             show the version
  --help                show this help
"""
  ErrCheckFailed = 1
  ErrParseInputFailed = 2
  ErrParseOutputFailed = 3
  ErrEqFailed = 4

proc writeHelp() =
  stdout.write(Usage)
  stdout.flushFile()
  quit(0)

proc writeVersion() =
  stdout.write(Version & "\n")
  stdout.flushFile()
  quit(0)

proc parse(input, filename: string, printTokens: bool, conf: ConfigRef): PNode =
  let fn = if filename == "-": "stdin" else: filename

  parseString(input, newIdentCache(), conf, fn, printTokens = printTokens)

func isNimFile(file: string): bool =
  ## Check if a file is a Nim file (i.e. ends in .nim/nims/nimble)
  let (_, _, ext) = file.splitFile()
  ext in [".nim", ".nims", ".nimble"]

proc makeConfigRef(): ConfigRef =
  let conf = newConfigRef()
  conf.errorMax = int.high
  conf

proc prettyPrint(infile, outfile: string, debug, check, printTokens: bool): int =
  let
    conf = makeConfigRef()
    input =
      if infile == "-":
        readAll(stdin)
      else:
        readFile(infile)
    node = parse(input, infile, printTokens, conf)

  if conf.errorCounter > 0:
    localError(
      conf, TLineInfo(fileIndex: FileIndex(0)), "Skipped file, input cannot be parsed"
    )

    return ErrParseInputFailed

  var output = renderTree(node, conf)
  if not output.endsWith("\n"):
    output.add "\n"

  if conf.errorCounter > 0:
    return ErrParseOutputFailed

  if infile != "-":
    if debug:
      # Always write file in debug mode
      writeFile(infile & ".nph.yaml", treeToYaml(nil, node))
      if infile != outfile:
        writeFile(outfile, output)
        writeFile(
          outfile & ".nph.yaml",
          treeToYaml(nil, parse(output, outfile, printTokens, newConfigRef())),
        )
    elif fileExists(outFile) and output == readFile(outFile):
      # No formatting difference - don't touch file modificuation date
      return QuitSuccess

  let eq = equivalent(input, infile, output, if infile == "-": "stdout" else: outfile)

  template writeUnformatted() =
    if not debug and (infile != outfile or infile == "-"):
      # Write unformatted content
      if not check:
        if infile == "-":
          write(stdout, input)
        else:
          writeFile(outfile, input)

  case eq.kind
  of Same:
    if check:
      ErrCheckFailed # We failed the equivalence check above
    else:
      # Formatting changed the file
      if not debug or infile == "-":
        if infile == "-":
          write(stdout, output)
        else:
          writeFile(outfile, output)

      QuitSuccess
  of ParseError:
    writeUnformatted()

    localError(
      conf,
      TLineInfo(fileIndex: FileIndex(0)),
      "Skipped file, formatted output cannot be parsed (bug! " & Version & ")",
    )

    ErrEqFailed
  of Different:
    writeUnformatted()

    stderr.writeLine "--- Input ---"
    stderr.writeLine input
    stderr.writeLine "--- Formatted ---"
    stderr.writeLine output
    stderr.writeLine "--- PRE ---"
    stderr.writeLine treeToYaml(nil, eq.a)
    stderr.writeLine "--- POST ---"
    stderr.writeLine treeToYaml(nil, eq.b)

    localError(
      conf,
      TLineInfo(fileIndex: FileIndex(0)),
      "Skipped file, formatted output does not match input (bug! " & Version & ")",
    )

    ErrEqFailed

proc main() =
  var
    outfile, outdir: string
    infiles = newSeq[string]()
    outfiles = newSeq[string]()
    debug = false
    check = false
    printTokens = false
    usesDir = false

  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if dirExists(key):
        usesDir = true
        for file in walkDirRec(key):
          if file.isNimFile:
            inFiles &= file
      else:
        infiles.add(key.addFileExt(".nim"))
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h":
        writeHelp()
      of "version", "v":
        writeVersion()
      of "debug":
        debug = true
      of "print-tokens":
        printTokens = true
      of "check":
        check = true
      of "output", "o", "out":
        outfile = val
      of "outDir", "outdir":
        outdir = val
      of "":
        infiles.add("-")
      else:
        writeHelp()
    of cmdEnd:
      assert(false) # cannot happen

  if infiles.len == 0:
    quit "[Error] no input file.", 3

  if outfile.len != 0 and outdir.len != 0:
    quit "[Error] out and outDir cannot both be specified", 3

  if outfile.len != 0 and usesDir:
    quit "[Error] out cannot be used alongside directories", 3

  if outfile.len == 0 and outdir.len == 0:
    outfiles = infiles
  elif outfile.len != 0 and infiles.len > 1:
    # Take the last file to maintain backwards compatibility
    let infile = infiles[^1]

    infiles = @[infile]
    outfiles = @[outfile]
  elif outfile.len != 0:
    outfiles = @[outfile]
  elif outdir.len != 0:
    outfiles = infiles.mapIt($(joinPath(outdir, it)))

  var res = QuitSuccess
  for (infile, outfile) in zip(infiles, outfiles):
    let (dir, _, _) = splitFile(outfile)

    createDir(dir)
    let err = prettyPrint(infile, outfile, debug, check, printTokens)
    case err
    of ErrCheckFailed:
      quit ErrCheckFailed
    else:
      # Keep going on source code errors but fail the program eventually
      res = max(res, err)

  quit res

when isMainModule:
  main()
