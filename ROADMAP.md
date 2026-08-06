# FortFront roadmap

FortFront owns parsing, semantic resolution, typed public queries, and
diagnostics for the ffc pipeline. It must remain backend-neutral: when ffc
needs a missing fact, fix or specify the public query here rather than exposing
private arena layout or adding an ffc text-name workaround.

## Current handoff (2026-08-06)

- The implementation baseline is the current tip of `main`; the older
  `5ff07184` snapshot recorded here has been superseded.
- The parser and semantic changes used by the current ffc tranche are on
  `main`; focused builds pass at the recorded baseline, while the full
  downstream ffc suite still has unrelated known failures.
- The maintained rejection workflow is mandatory. A rejection change must
  prove that intended invalid cases are rejected and that valid corpus files
  are not newly rejected.

## Active blockers and cross-references

- [#2883](https://github.com/lazy-fortran/fortfront/issues/2883) defines the
  explicit-interface rejection contract. Coordinate its serial landing with
  ffc [#584](https://github.com/lazy-fortran/ffc/issues/584).
- [#2924](https://github.com/lazy-fortran/fortfront/issues/2924) owns the
  rejection corpus gate. It is the safety gate for all later rejection issues
  and for ffc [#663](https://github.com/lazy-fortran/ffc/issues/663).
- [#2951](https://github.com/lazy-fortran/fortfront/issues/2951) protects valid
  programs from semantic over-tightening; ffc must not paper over regressions.
- [#2994](https://github.com/lazy-fortran/fortfront/issues/2994) is the current
  Lazy specialization ABI blocker and is coupled to ffc [#437](https://github.com/lazy-fortran/ffc/issues/437)
  and [#433](https://github.com/lazy-fortran/ffc/issues/433).
- The silent-source-drop family (#2966, #2967, #2972, #2974, and #2977) needs
  structural scanner coverage, not isolated corpus-only exceptions.

## Delivery gate

Each change needs an independent parser/semantic or compiler-behavior oracle,
the focused tests, the rejection gate when diagnostics change, and the full
repository `fo` pipeline. Update this file when a linked issue or public query
changes state.

The `fo` source scanner currently schedules one module per source file. The
frontend helper implementation therefore keeps each module in its own file;
this is a build-order contract, not a compiler-specific workaround. A split
of the former two-module helper source was verified by a cold `fo build`.

The compiler lanes are distinct: GNU Fortran, NVIDIA `nvfortran`, Intel LLVM
`ifx`, and LLVM Flang are supported targets; legacy Intel `ifort` is not. The
current portability tranche removes several GNU-tolerated constructs and
compiler-sensitive internal representations. FortFront's source units now
compile with GNU and `nvfortran` 26.5, and its focused expression/control-flow
oracles pass with GNU. A cold `nvfortran` build now completes all 381 targets;
the former raw-backslash failures in path validation and the CLI are fixed.
The downstream FortAD `nvfortran` gate still needs a fresh run against this
revision; its earlier ICE in `fortad_lower.f90` is not a FortFront build
failure.

## Outstanding work (2026-08-06)

### Nested-construct dispatch: what was fixed and why it mattered

Four gaps shared one shape. A construct nested inside a loop body, an if
body, or a case arm reaches a statement dispatcher whose caller populated
no callback for that construct, and the dispatcher answered "unrecognized
statement" rather than reaching a parser. One unrecognized statement fails
the whole file, so a single missing callback took out a source entirely.

Fixed on `main`:

- `select type` and `select case` had no fallback. This was by far the
  largest single cause: across fortfront's own tree it accounted for about
  eighty files.
- The `block` construct was not handled by the nested dispatcher at all,
  and its statement slice stopped at the header, so its declarations and
  its `end block` were parsed as loose statements. Three separate body
  parsers keep their own slicing loops (`parse_statement_body`, the do
  parser, the if parser) and each needed the extension. `block data` is
  excluded: it is a program unit, not a construct.
- A `use` statement inside a `block` construct was unhandled.
- `stop 1, quiet=.true.` ended the statement at the comma, because
  `statement_contains_assignment` counted the `=` of the F2018 `quiet=`
  specifier as an assignment operator and concluded `stop` was a variable
  being assigned to. An assignment's `=` always precedes any top-level
  comma, so the scan now stops there.

Result at the time: fortfront sources that failed to parse dropped from about
100 to 15. A 2026-08-06 re-audit found that all 15 now parse and resolve on
current `main`; none remains a cold-build blocker.

### Former own-source parse blockers: cleared

The 15 files listed in the previous roadmap snapshot were each checked with
the compiler-facing frontend probe. Every file returned `parse_ok` and
`semantic_ok`, including `src/utilities/path_validation.f90` and both
compiler-query modules. `fo` now discovers and builds all 381 targets on the
GNU lane. Keep a cold source-discovery run in the delivery gate so a future
silent drop cannot regress into a late undefined reference.

### Lexer: a comment inside a continuation line

A full-line comment between the continuation lines of one statement is
legal Fortran and gfortran accepts it. fortfront reports "Unexpected token
newline in expression". Hit in fortfront's own
`parser_keyword_disambiguation.f90`, where an interleaved comment inside a
continued `select case` list made the file unparseable; worked around by
moving the comments above the statement. The lexer gap itself is unfixed.

### Lexer: a character literal continued across lines: fixed

A format string split across a continuation is mis-lexed as code:

```fortran
write (unit, "(a,',',i0,',',es24.16e3,',', &
    es24.16e3)") "ring", a, x, y
```

The lexer now keeps the character context across the newline, removes the
continuation syntax without removing value blanks, and accepts the common
GNU extension that omits the leading ampersand. A lexer oracle checks the
resulting character token, the issue-2254 example checks parse/emission, and
fortfem's `example/iga_polar_feec/iga_polar_feec.f90` now returns `parse_ok`
and `semantic_ok` at the former failures on lines 328 and 344.

### Test status on current `main`

The former 25-test GNU failure list is cleared. The local GNU gate builds all
381 targets and all 378 test programs; the suite contains 483 tests.
`test_module_distribution` is still parallel-fragile because it invokes the
repository Makefile and cleans shared artifacts; it passes alone, but can fail
when scheduled beside the full suite. The final bare gate passed, but the test
should be isolated or made artifact-private.

The Windows Actions lane still has six red tests:

- `test_compiler_facing_queries`
- `test_reject_bind_02_diagnostics`
- `test_reject_placement_01_diagnostics`
- `test_reject_value_scope_01_diagnostics`
- `test_all_examples_slow`
- `test_elemental_validation`

The recurring Windows-only symptom is a false diagnostic that a `VALUE`
entity is not a dummy argument. The placement rejection also reports an
impossible leading byte (`0xE0`), consistent with compiler-sensitive arena
copy or lifetime corruption. These need a Windows runtime oracle; do not hide
them with platform-specific expected output.

### Consumers to re-verify after any parser change

fo pins fortfront `main`, so a fortfront regression reaches fo, and through
fo reaches every downstream build. Any change here must be checked against
fo's own sources and fortfem's, not only fortfront's tree. Note that fpm
caches the dependency clone: `rm -rf build/dependencies build/cache.toml`
before reinstalling fo, or the old fortfront is silently reused.

## Segfault under gfortran 13.3: an if block inside a case arm

Sixteen lines reproduce it, and this is the whole thing:

```fortran
module m
    implicit none
    integer, parameter :: dp = kind(1.0d0)
contains
    function apply(head, x) result(v)
        character(len=*), intent(in) :: head
        real(dp), intent(in) :: x
        real(dp) :: v
        v = 0.0_dp
        select case (head)
        case ("sqrt")
            if (x < 0.0_dp) then
                v = 0.0_dp
            else
                v = sqrt(x)
            end if
        case default
            v = -1.0_dp
        end select
    end function apply
end module m
```

Parsing that segfaults under gfortran 13.3, which is what Ubuntu 24.04
and so every GitHub runner has. Under a newer gfortran it parses, so
this is invisible on a developer machine.

Take the `if` out and it parses. `case ("abs"); v = abs(x)` on one line
parses. The number of case arms does not matter - forty-eight plain
arms are fine. It needs a block `if` inside a `case` arm.

Why it matters beyond fortfront: `fo` bundles fortfront, fortnum's
codegen tool depends on fortsym, and eight of fortsym's sources contain
this shape. That is why two of fortnum's CI jobs fail, and they fail on
main as well - nothing to do with what is being built.

### What has been ruled out

Each of these was tested, not argued:

- The `fortsym.lock` revision, and fortsym itself: cloned at the locked
  revision and run clean.
- `fo`'s own revision: rebuilt exactly as CI does, clean.
- Stack exhaustion: `ulimit -s unlimited` still crashes.
- Array bounds: `-fcheck=all` reports no violation.
- An out-of-bounds read in `extract_statement_tokens`: real, fixed, and
  not this - patching it into the pinned source still crashed.
- Slicing a construct at its header in `parse_statement_body`: real,
  fixed, and not this either - the fix is present in the built
  dependency and the sixteen lines still crash.

Three tools disagree about where it dies. gdb says
`handle_control_keyword` transfers to address 5, AddressSanitizer says
a bad eight-byte load in `fo`'s `build_dag_from_units`, and
`-fcheck=all` says nothing is out of range. Disagreement of that kind
means memory is corrupted earlier and the crash surfaces wherever the
damage is next touched, so the reported site is not the bug. What is
needed is the write, not the read.

### Reproducing

`scripts` for this live in the session scratchpad; the shape is:

    docker run --rm -v <dir>:/w -w /w ubuntu:24.04 bash /w/verify2.sh

which builds a small fortfront harness and parses one file per run,
reporting rc=139 for a crash. Install gfortran in any container that
runs the harness - without libgfortran the binary exits 127 and a
bisect will happily converge on nonsense.



## One fortfem source does not parse

563 of 564. Sixteen at the start of this work, and every one of the
fifteen closed was a case of Fortran reserving no words - all of them
invisible until the segfault in the statement callbacks was fixed,
because the garbage pointer made `associated` answer true and the
parser reached a real parser by accident.

Closed: a keyword as a declared entity name; `parameter` and `rank` as
assignment targets; a procedure named with a keyword; a bare `do`
inside a `do while`; and `block` as a variable.

The `block` case is worth remembering. Four guards were added in four
plausible places - the span locator, the disambiguation list, the
dispatcher's own case, and component references - and none of them
moved the error, because none was on the path. The site that actually
consumed the token was the procedure-body parser, which routes a
leading `block` into `parse_block_construct` without asking whether it
is being assigned to. Finding it took reading the dispatch order rather
than adding a fifth guard.

**`src/triangle_compat/tc_locate.f90:66`** is what is left. The
reported column is past the end of that line, so the failure is at a
statement boundary rather than in the statement itself. The block
around it nests four `if`s and contains a four-line continued call.
Neither reproduces on its own. Reducing it needs care: cutting the
block out by line range leaves unbalanced `end if`s and produces a
different error that looks like progress and is not.
