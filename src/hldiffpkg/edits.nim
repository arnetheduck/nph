## Module to get edits across two `openArray[T]` where `T` has `hash` & `==`.
## Algorithm is Python's `difflib.SequenceMatcher` follow-on to Ratcliff1988's
## edit distance with run time ~ `[o(n), O(n^2)]` where `n = max(s.len, t.len)`.

import std/[tables, algorithm, sets, sequtils, heapqueue]

type
  EdKind* = enum
    ekEql
    ekDel
    ekIns
    ekSub

  Same* = tuple[st: Slice[int], n: int] ## start in each & len of match
  Edit* = tuple[ek: EdKind, s, t: Slice[int]] ## 2-seq edit op & args
  Cmper*[T] = object ## state used to compute edits
    junk: HashSet[T]
    t2js: Table[T, seq[int]]

proc eoa(x: Same): int {.inline.} =
  x.st.a + x.n # A few Same utility procs

proc eob(x: Same): int {.inline.} =
  x.st.b + x.n # End Of A|B & comparison.

proc `<`(x, y: Same): bool {.inline.} =
  x.st.a < y.st.a or x.st.b < y.st.b

proc init*[T](
    c: var Cmper[T], s, t: openArray[T], junk: HashSet[T] = initHashSet[T]()
) =
  ## Re-init `c`.  `junk` cannot start a `Same`.  `s` is unused, but present for
  ## consistency with other calls.
  c.junk = junk
  for j, key in t: # chain `t`
    if key notin junk:
      c.t2js.mgetOrPut(key, default(seq[int])).add(j)

proc initCmper*[T](s, t: openArray[T], junk: HashSet[T] = initHashSet[T]()): Cmper[T] =
  ## Make a new `Cmper` & compute some metadata.  `junk` cannot start a `Same`.
  ## `s` is unused, but present for consistency with other calls.
  result.init(s, t, junk)

proc lcs[T](
    c: var Cmper[T], s, t: openArray[T], x, y: Slice[int], n0, n1: var seq[int]
): Same =
  # Return longest common subseq between the two seqs, within given s/t slices.
  result = (x.a .. y.a, 0) # init result
  var r = result # `template r=result` fails
  var n0p = n0.addr
  var n1p = n1.addr
  n0[0].addr.zeroMem n0[0].sizeof * n0.len
  for i in x.a ..< x.b: # Look at all s[i] in t
    n1p[][0].addr.zeroMem n1[0].sizeof * n1.len
    for j in c.t2js.getOrDefault(s[i]):
      if j < y.a:
        continue
      if j >= y.b:
        break
      let k = n0p[][j] + 1
      n1p[][j + 1] = k
      if k > r.n:
        r = (i - k + 1 .. j - k + 1, k)
    swap n0p, n1p
  while r.st.a > x.a and r.st.b > y.a and t[r.st.b - 1] notin c.junk and
      s[r.st.a - 1] == t[r.st.b - 1]:
    dec r.st.a
    dec r.st.b
    inc r.n # Extend non-junk @front
  while r.eoa < x.b and r.eob < y.b and t[r.eob] notin c.junk and s[r.eoa] == t[r.eob]:
    inc r.n # Extend non-junk @back
  while r.st.a > x.a and r.st.b > y.a and t[r.st.b - 1] in c.junk and
      s[r.st.a - 1] == t[r.st.b - 1]:
    dec r.st.a
    dec r.st.b
    inc r.n # Extend junk @front
  while r.eoa < x.b and r.eob < y.b and t[r.eob] in c.junk and s[r.eoa] == t[r.eob]:
    inc r.n # Extend junk @back
  result = r

proc sames*[T](c: var Cmper[T], s, t: openArray[T]): seq[Same] =
  ## Return every `Same` across the two seqs.  Access via `iterator edits`.
  var blocks: seq[Same] # Preliminary sames
  var n0 = newSeq[int](max(s.len, t.len) + 1)
  var n1 = n0
  var q = @[(0 .. s.len, 0 .. t.len)]
  while q.len > 0:
    let (x, y) = q.pop
    let m = c.lcs(s, t, x, y, n0, n1) # s[m.a..<m.eoa]==t[m.b..<m.eob]
    if m.n > 0:
      blocks.add m
      if x.a < m.st.a and y.a < m.st.b:
        q.add (x.a .. m.st.a, y.a .. m.st.b)
      if m.eoa < x.b and m.eob < y.b:
        q.add (m.eoa .. x.b, m.eob .. y.b)
  blocks.sort # iterator edits & clients need ix order, not recursion order
  var m1: Same # init to dummy
  for m2 in blocks: # Merge adjacent equal blocks
    if m1.eoa == m2.st.a and m1.eob == m2.st.b: # Adjacent; merge
      m1.n += m2.n # Extend m1 by length of m2
    else:
      if m1.n != 0:
        result.add m1
        # 1st in result if not dummy
      m1 = m2
  if m1.n != 0:
    result.add m1
  result.add (s.len .. t.len, 0) # Add (EOA..EOB,0) terminator

iterator edits*(sames: seq[Same]): Edit =
  ## Yield edits needed to transform `seq s` into `t`, given `sames`.
  var i, j: int
  for m in sames: # Classify edits & yield
    if i < m.st.a and j < m.st.b:
      yield (ekSub, i ..< m.st.a, j ..< m.st.b)
    elif i < m.st.a:
      yield (ekDel, i ..< m.st.a, j ..< m.st.b)
    elif j < m.st.b:
      yield (ekIns, i ..< m.st.a, j ..< m.st.b)
    i = m.eoa
    j = m.eob
    if m.n != 0:
      yield (ekEql, m.st.a ..< i, m.st.b ..< j)

proc sames*[T](s, t: openArray[T], junk: HashSet[T] = initHashSet[T]()): seq[Same] =
  ## Return every `Same` across the two seqs.  Access via `iterator edits`.
  var c = initCmper[T](s, t, junk)
  return c.sames(s, t)

iterator edits*[T](s, t: openArray[T], junk: HashSet[T] = initHashSet[T]()): Edit =
  ## Convenience wrapper around `edits(s, t, seq[Same])`.
  var c = initCmper[T](s, t, junk)
  for edit in edits(c.sames(s, t)):
    yield edit

proc grouped*(ss: seq[Same], n = 3): seq[seq[Edit]] =
  ## Yield groups of edits with up to `n` lines of context; Usable for unidiffs.
  let n = max(0, n - 1)
  var all, grp: seq[Edit]
  for ed in edits(ss):
    all.add(ed) # collect all edits
  if all.len == 0: # fix empty seq
    all.add (ekEql, 0 ..< 1, 0 ..< 1)
  if all[0].ek == ekEql: # fix leading if no diff
    all[0].s.a = max(all[0].s.a, all[0].s.b - n)
    all[0].t.a = max(all[0].t.a, all[0].t.b - n)
  if all[^1].ek == ekEql: # fix trailing if no diff
    all[^1].s.b = min(all[^1].s.b, all[^1].s.a + n)
    all[^1].t.b = min(all[^1].t.b, all[^1].t.a + n)
  for ed in all:
    var e = ed
    if ed.ek == ekEql and ed.s.b - ed.s.a > 2 * n: # big range w/no diff
      grp.add (
        ekEql, ed.s.a ..< min(ed.s.b, ed.s.a + n), ed.t.a ..< min(ed.t.b, ed.t.a + n)
      )
      result.add grp[0 ..^ 1] # end current & start new
      grp.setLen 0
      e.s.a = max(ed.s.a, ed.s.b - n)
      e.t.a = max(ed.t.a, ed.t.b - n)
    grp.add e
  if grp.len > 0 and not (grp.len == 1 and grp[0].ek == ekEql):
    result.add grp

proc count[T](s: openArray[T]): Table[T, int] =
  result = initTable[T, int](s.len div 2)
  for e in s:
    result.mgetOrPut(e, 0).inc

proc autojunk*[T](s: openArray[T], ajDiv = 0): seq[T] =
  ## Return items with repetition count `>= thresh = s.len/ajDiv + 1`; Empty if
  ## `ajDiv <= 0` or `thresh < 3`.
  if ajDiv <= 0:
    return
  let thresh = s.len div ajDiv + 1
  if thresh < 3:
    return
  for e, n in pairs(s.count):
    if n >= thresh:
      result.add e

proc similarity*(sames: seq[Same]): int =
  ## Return a similarity score with max value = `s.len + t.len`.
  for m in sames:
    result += m.n

proc similarity*[T](s, t: openArray[T], junk: HashSet[T] = initHashSet[T]()): int =
  ## Return a similarity score with max value = `s.len + t.len`.
  var c = initCmper[T](s, t, junk)
  c.sames(s, t).similarity

proc similUB1*[T](s, t: openArray[T]): int =
  ## Return cardinality(multiset-intersection), a fast `similarity` upper bound.
  let tCnt = t.count
  var avail: Table[T, int] # Times `e` appears in `t` minus seen so far in `s`
  for e in s:
    let n =
      if e in avail:
        avail[e]
      else:
        tCnt.getOrDefault(e, 0)
    avail[e] = n - 1
    if n > 0:
      result.inc

proc closeTo*(e: string, s: seq[string], n = 3, percent = 59): seq[string] =
  ##Return up to `n` members of `s` "close to" (`similarity > percent/100`) `e`,
  ##closest first.  `"appel".closeTo(@["ape","apple","pot"])==@["apple","ape"]`
  var hq = initHeapQueue[tuple[score: int, str: string]]()
  let m = percent * (s.len + 1) div 100 # min score to include
  var c = initCmper("", e) # Arg1 doesn't matter;Arg2 const
  for x in s:
    if min(x.len, e.len) > m and similUB1(x, e) > m: # check upper bounds first
      if (let r = c.sames(x, e).similarity; r) > m: # then expensive similarity
        let z = (r, x) # above thresh->bnded HeapQueue
        if hq.len < n:
          hq.push z
        elif z > hq[0]:
          discard hq.replace(z)
  while hq.len > 0:
    result.add hq.pop.str
    # pop off in increasing order..
  result.reverse # ..then reverse.

proc rangeUni*(start, stop: int): string {.inline.} =
  var begin = start + 1 # 1-origin; www.unix.org/single_unix_specification
  let size = stop - start
  if size == 1:
    return $begin
  if size == 0:
    begin -= 1 # empty ranges begin at line just before the range
  return $begin & "," & $size

when isMainModule:
  import std/[os, times, strutils], cligen
  when not declared(File):
    import std/syncio

  proc lines(path: string): seq[string] =
    open(path).readAll.strip.split('\n')

  proc diffu*(ajDiv = 0, junk = "", n = 3, paths: seq[string]): int =
    ## This is a sample/test program printing a unified diff format delta from
    ## source $1 to target $2 with optional Py difflib non-junk alignment.
    if paths.len != 2:
      stderr.write "Need 2 paths; --help for help\n"
      return 1
    let s = paths[0].lines
    let t = paths[1].lines
    let jnk = toHashSet(
      if junk.len > 0:
        junk.lines
      else:
        autojunk(t, ajDiv)
    )
    var begun = false
    for eds in grouped(sames(s, t, jnk), n):
      if not begun:
        begun = true
        const tf = "yyyy-MM-dd HH:mm:ss zzz" # nim times bug .fff => '.' invalid
        echo "--- ", paths[0], '\t', paths[0].getLastModificationTime.format(tf)
        echo "+++ ", paths[1], '\t', paths[1].getLastModificationTime.format(tf)
      echo "@@ -",
        rangeUni(eds[0].s.a, eds[^1].s.b + 1),
        " +",
        rangeUni(eds[0].t.a, eds[^1].t.b + 1),
        " @@"
      for ed in eds:
        case ed.ek
        of ekEql:
          (;
            for ln in s[ed.s]:
              echo " ", ln
          )
        of ekDel:
          (;
            for ln in s[ed.s]:
              echo "-", ln
          )
        of ekIns:
          (;
            for ln in t[ed.t]:
              echo "+", ln
          )
        of ekSub:
          for ln in s[ed.s]:
            echo "-", ln
          for ln in t[ed.t]:
            echo "+", ln
    result = if begun: 1 else: 0

  dispatch(
    diffu,
    cmdName = "diffu",
    help = {
      "junk": "Use lines in this file to apply Py difflib heuristic",
      "ajDiv": "auto-junk heuristic popularity divisor (Py uses 100)",
      "n": "number of context lines for the unified diff",
    },
  )
