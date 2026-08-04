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



## Sixteen fortfem sources do not parse

548 of fortfem's 564 sources parse; these sixteen do not, and they are
what keeps fortfem's build red under a current fo. None of it is new -
the segfault in the statement callbacks used to mask all of it, because
a garbage pointer made `associated` answer true and the parser reached
a real parser by accident. Fixing the crash turned silent wrong
behaviour into honest errors, and these are the honest errors.

Three shapes, in order of how many files they account for.

**A keyword as a name in a declaration list.** Fortran reserves no
words, so these are legal:

    real(dp) :: distance, parameter, source(2)
    integer :: patch, interface, trace, offset

The entity-list parser stops at `parameter`, leaves the rest of the
line unconsumed, and `reject_unconsumed_tokens` reports "unexpected
token after statement" from a position well past the real problem.
Fixing it means accepting a keyword as an entity name there. Two
earlier attempts aimed at `keyword_should_parse_as_identifier` had no
effect at all: that function is not on this path.

**`do` unrecognised.** Nine occurrences. The fallback now covers both
the counted and the while form, which did not move these, so they
arrive through a third path that has not been found.

**One block construct whose end is not located**, in
equation_objective_registry.f90 at line 151.

The failing files and their first error:

```
src/bem/helmholtz_boundary_operators_2d.f90 :: ERROR at line 468, column 23: Unrecognized statement: parameter = nodes
src/bem/helmholtz_exterior_2d.f90 :: ERROR at line 317, column 57: Syntax error: unexpected token after statement
src/elements/bspline_multipatch.f90 :: ERROR at line 167, column 36: Syntax error: unexpected token after statement
src/fortfem_api_spaces.f90 :: 
src/geometry/fci_terminal_segment_2d.f90 :: ERROR at line 33, column 53: Syntax error: unexpected token after statement
src/geometry/fci_terminal_triangle_3d.f90 :: ERROR at line 84, column 23: Unrecognized statement: parameter = numerator
src/mesh/triangle_io.f90 :: ERROR at line 110, column 17: Unrecognized statement: do 
src/operators/equation_objective_registry.f90 :: ERROR at line 151, column 9: internal: could not locate the end of this block construct; refusing to drop the statements that follow
src/operators/fci_boundary_patch_mortar.f90 :: ERROR at line 364, column 13: Unrecognized statement: rank = rank
src/solvers/pseudo_arclength_residual.f90 :: ERROR at line 24, column 65: Syntax error: unexpected token after statement
src/topology/cell_complex.f90 :: ERROR at line 527, column 13: Unrecognized statement: rank = rank
src/topology/distributed_trace_ownership.f90 :: ERROR at line 100, column 53: Syntax error: unexpected token after statement
src/topology/mpi_trace_exchange.f90 :: ERROR at line 79, column 58: Syntax error: unexpected token after statement
src/triangle_compat/tc_enforce.f90 :: ERROR at line 123, column 29: Unrecognized statement: do 
src/triangle_compat/tc_locate.f90 :: ERROR at line 153, column 17: Unrecognized statement: do 
src/triangle_compat/tc_sort.f90 :: ERROR at line 36, column 17: Unrecognized statement: do
```

Reproduce one file at a time with a small harness over
`compile_frontend_from_string`, checking `res%parse_ok` rather than the
exit status. A file can fail to parse without crashing, and counting
crashes alone reports these as fine - which is exactly how they stayed
hidden.
