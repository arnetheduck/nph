#           nph
#        (c) Copyright 2023 Jacek Sieka
## Opinionated source code formatter

import
  "."/[
    astcmp, astyaml, phast, phastyaml, phmsgs, phlineinfos, phoptions, phparser,
    phrenderer,
  ]

import "$nim"/compiler/idents

import std/[parseopt, strutils, os, sequtils, tables]
import pkg/regex
import pkg/toml_serialization

static:
  doAssert NimMajor == 2 and NimMinor == 2, "nph needs a specific version of Nim"

type
  NphConfig = object
    exclude: seq[string]
    extendExclude {.serializedFieldName("extend-exclude").}: seq[string]
    includePatterns {.serializedFieldName("include").}: seq[string]

  CompiledPatterns = object
    excludePatterns: seq[Regex2]
    includePatterns: seq[Regex2]

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
  --exclude:pattern     regex pattern for files/dirs to exclude (overrides defaults)
  --extend-exclude:pattern  regex pattern to add to default exclusions
  --include:pattern     regex pattern for files to include (default: \.nim(s|ble)?$)
  --config:file         config file to use (default: .nph.toml if it exists)
  --version             show the version
  --help                show this help
"""
  DefaultExcludePatterns = [
    r"\.git", r"\.hg", r"\.svn", r"\.nimble", r"nimbledeps", r"vendor", r"nimcache",
    r"\.vscode", r"\.idea", r"__pycache__", r"node_modules", r"\.mypy_cache",
    r"\.pytest_cache", r"\.nox", r"\.tox", r"\.venv", r"venv", r"\.eggs", r"_build",
    r"buck-out", r"build", r"dist",
  ]
  DefaultIncludePattern = r"\.nim(s|ble)?$"
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

proc loadConfig(configFile: string): NphConfig =
  result = NphConfig(exclude: @[], extendExclude: @[], includePatterns: @[])

  if not fileExists(configFile):
    return

  try:
    result = Toml.loadFile(configFile, NphConfig)
  except CatchableError as e:
    stderr.writeLine "Warning: Failed to parse config file: " & configFile & " (" & e.msg &
      ")"
    discard

func normalizePath(path: string): string =
  ## Normalize path to use forward slashes for cross-platform regex matching
  ## Following Black's approach: convert all backslashes to forward slashes
  path.replace("\\", "/")

proc compilePatterns(patterns: seq[string]): seq[Regex2] =
  result = newSeq[Regex2]()
  for pattern in patterns:
    try:
      result.add(re2(pattern))
    except RegexError as e:
      stderr.writeLine "Warning: Invalid regex pattern '" & pattern & "': " & e.msg

func shouldExclude(path: string, excludePatterns: seq[Regex2]): bool =
  let normalizedPath = normalizePath(path)
  excludePatterns.anyIt(normalizedPath.contains(it))

func shouldInclude(path: string, includePatterns: seq[Regex2]): bool =
  if includePatterns.len == 0:
    return true
  let normalizedPath = normalizePath(path)
  includePatterns.anyIt(normalizedPath.contains(it))

func matchesFilters(path: string, patterns: CompiledPatterns): bool =
  if shouldExclude(path, patterns.excludePatterns):
    return false
  if not shouldInclude(path, patterns.includePatterns):
    return false
  return true

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
      writeFile(infile & ".nph.yaml", treeToYaml(nil, node) & "\n")
      if infile != outfile:
        writeFile(outfile, output)
        writeFile(
          outfile & ".nph.yaml",
          treeToYaml(nil, parse(output, outfile, printTokens, newConfigRef())) & "\n",
        )
    elif fileExists(outfile) and output == readFile(outfile):
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
    outfile, outdir, configFile: string
    infiles = newSeq[string]()
    explicitFiles = newSeq[string]() # Files passed explicitly, not from dir walk
    outfiles = newSeq[string]()
    debug = false
    check = false
    printTokens = false
    usesDir = false
    cliExclude = newSeq[string]()
    cliExtendExclude = newSeq[string]()
    cliInclude = newSeq[string]()

  # Default config file location
  configFile = ".nph.toml"

  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if dirExists(key):
        usesDir = true
        for file in walkDirRec(key):
          if file.isNimFile:
            infiles &= file
            explicitFiles.add(file) # Track files from explicit directories
      else:
        let f = key.addFileExt(".nim")
        infiles.add(f)
        explicitFiles.add(f) # Track explicitly passed files
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
      of "exclude":
        cliExclude.add(val)
      of "extend-exclude", "extendexclude":
        cliExtendExclude.add(val)
      of "include":
        cliInclude.add(val)
      of "config":
        configFile = val
      of "":
        let f = "-"
        infiles.add(f)
        explicitFiles.add(f) # stdin is explicit
      else:
        writeHelp()
    of cmdEnd:
      assert(false) # cannot happen

  # Load config from file
  var config = loadConfig(configFile)

  # CLI options override config file
  # exclude and include completely replace config
  if cliExclude.len > 0:
    config.exclude = cliExclude
  if cliInclude.len > 0:
    config.includePatterns = cliInclude
  # extend-exclude adds to config patterns
  if cliExtendExclude.len > 0:
    config.extendExclude.add(cliExtendExclude)

  # Build final exclude patterns: defaults + extend-exclude, or just exclude if set
  var finalExcludePatterns: seq[string]
  if config.exclude.len > 0:
    # User specified exclude patterns - replace defaults
    finalExcludePatterns = config.exclude
  else:
    # Use defaults + any extend-exclude patterns
    finalExcludePatterns = @DefaultExcludePatterns
    finalExcludePatterns.add(config.extendExclude)

  # Build final include patterns
  var finalIncludePatterns: seq[string]
  if config.includePatterns.len > 0:
    finalIncludePatterns = config.includePatterns
  else:
    finalIncludePatterns = @[DefaultIncludePattern]

  # Pre-compile regex patterns for faster matching
  let compiledPatterns = CompiledPatterns(
    excludePatterns: compilePatterns(finalExcludePatterns),
    includePatterns: compilePatterns(finalIncludePatterns),
  )

  # Filter input files based on include/exclude patterns
  # BUT: explicitly passed files bypass filtering (like Black)
  var filteredFiles = newSeq[string]()
  for file in infiles:
    if file in explicitFiles or matchesFilters(file, compiledPatterns):
      filteredFiles.add(file)

  infiles = filteredFiles

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
