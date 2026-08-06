# FortFront roadmap

FortFront owns parsing, semantic resolution, typed public queries, and
diagnostics for the ffc pipeline. It must remain backend-neutral: when ffc
needs a missing fact, fix or specify the public query here rather than exposing
private arena layout or adding an ffc text-name workaround.

## Current handoff (2026-08-03)

- The implementation baseline is `5ff07184`; the roadmap commits are pushed
  on current `main`.
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
oracles pass with GNU. The `nvfortran` cold executable/link gate remains open:
module-procedure symbols from the semantic analyzer are unresolved at link
time, and the downstream FortAD gate still ICEs in `fortad_lower.f90`.

## Outstanding work (2026-08-05)

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

Result: fortfront sources that fail to parse dropped from about 100 to 15.
`fpm test` stayed at 25 failures with an identical failing set before and
after, verified by diffing the sorted lists rather than comparing counts.

### The 15 sources that still do not parse

These block `fo` from cold-building fortfront: a source `fo` cannot scan is
dropped from the module DAG and never compiled, so the failure surfaces
later as an undefined reference at link time rather than as a parse error.
See the fo roadmap for that mechanism.

Bare `end` or `end block` reported as unrecognized, which means a construct
slice still ends before its terminator on some path:

- `src/parser/expressions/parser_array_constructs.f90` line 426
- `src/parser/procedures/parser_result_types.f90` line 253
- `src/parser/statements/parser_statement_data_module.f90` line 605
- `src/parser/statements/parser_statement_utilities.f90` line 564
- `src/semantic/analyzers/semantic_binary_operations.f90` line 145
- `src/semantic/analyzers/semantic_procedure_signature.f90` line 164
- `src/semantic/type_hierarchy.f90` line 205
- `src/standardizers/standardizer_declarations_inference.f90` line 180
- `src/standardizers/standardizer_parameter.f90` line 128
- `src/utilities/fortfront_utils.f90` line 454

Other shapes, one each:

- `app/fortfront.f90` line 377: `flush (output_unit)` is not recognized.
- `src/frontend/frontend_compiler_queries.f90` line 565 and
  `src/frontend/frontend_compiler_type_queries.f90` line 1008: a statement
  beginning `operator` is not recognized.
- `src/semantic/analyzers/semantic_external_declaration_names.f90` line 138:
  an identifier beginning `block_` is mistaken for the `block` keyword.
- `src/utilities/path_validation.f90` line 296: reported as an IF construct
  missing its `then`.

Important caveat for whoever picks this up. Minimal reproductions of the
obvious shapes all pass: a `block` holding a `do` inside a `do`, and an
`if` inside a `select type` arm both parse. So these are not one shared
cause and cannot be closed by another fallback registration. Each needs
per-file bisection down to the construct that actually fails.

### Lexer: a comment inside a continuation line

A full-line comment between the continuation lines of one statement is
legal Fortran and gfortran accepts it. fortfront reports "Unexpected token
newline in expression". Hit in fortfront's own
`parser_keyword_disambiguation.f90`, where an interleaved comment inside a
continued `select case` list made the file unparseable; worked around by
moving the comments above the statement. The lexer gap itself is unfixed.

### Lexer: a character literal continued across lines

A format string split across a continuation is mis-lexed as code:

```fortran
write (unit, "(a,',',i0,',',es24.16e3,',', &
    es24.16e3)") "ring", a, x, y
```

The continued part comes back as the tokens `es24 . 16e3`, and the write
parser then reports "Expected ')' after write unit and format". Confirmed
present on `dfc442d4`, so it predates the nested-construct work above.
Reproduces on fortfem's `example/iga_polar_feec/iga_polar_feec.f90`
lines 328 and 344.

### 25 failing tests on `main`

Pre-existing and unrelated to the parser work above; the set is identical
before and after it. Grouped by theme:

- DATA statements: `test_issue_1405_data_statement`,
  `test_issue_1746_data_repeat_counts`,
  `test_issue_1899_data_multi_objects`, `test_issue_1899_data_scalars`,
  `test_issue_2251_data_implied_do`,
  `test_issue_2252_data_value_implied_do`,
  `test_issue_2349_data_boz_literals`,
  `test_issue_2349_data_trailing_commas`,
  `test_issue_2596_data_statement_scalar_too_many_values`
- BLOCK DATA program units: `test_issue_1578_block_data`,
  `test_issue_1900_block_data_after_program`,
  `test_issue_1900_block_data_labels`
- Derived types and type-bound procedures: `test_derived_type_extends`,
  `test_derived_type_extends_codegen`, `test_derived_type_parsing`,
  `test_extends_with_attributes`, `test_type_bound_procedures_codegen`,
  `test_type_contains_bindings`
- Remaining: `test_all_examples`, `test_issue_1610_recursive_pointers`,
  `test_issue_2281_header_only_declarations`,
  `test_issue_517_multi_unit_parsing`,
  `test_lfortran_traits_requirements_implements_parsing`,
  `test_reject_bind_02_diagnostics`,
  `test_variable_usage_block_construct`

`test_variable_usage_block_construct` is worth checking first now that the
BLOCK construct reaches a parser from nested bodies.

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
