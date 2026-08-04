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



## Six fortfem sources do not parse

Down from sixteen. Two fixes closed ten of them: accepting a keyword as
a declared entity name, and letting `parameter` and `rank` name a
variable in an assignment. Both are cases of Fortran reserving no
words, and both were masked until the segfault in the statement
callbacks was fixed.

Worth recording about the second: it was tried once, measured as having
no effect, and reverted. That measurement was right at the time - the
declaration on an earlier line failed first, so the file never reached
the assignment. It only worked once the declaration fix landed. Order
mattered, not the edit.

What is left.

**A bare `do` inside a `do while` body**, four files. A bare `do` on its
own parses; nested in a `do while` it does not. The body slicing is
fine - `find_matching_end_do` takes the whole construct - and the
sliced statement reaches `handle_control_keyword`, which has no do
callback there and falls back to `parse_do_loop`. That routine handles
`while` and `concurrent` and then calls `parse_standard_control`, which
fails when there is no control at all. So `parse_do_loop` has no
infinite-loop path, and the bare `do` that parses at statement level
gets there another way. Either give `parse_do_loop` that path or point
the fallback at whatever the statement-level dispatcher uses.

Minimal reproduction:

    do while (i < n)
        do
            i = i + 1
            if (i > n) exit
        end do
    end do

**One block construct whose end is not located**, in
equation_objective_registry.f90 at line 151, and **one file that fails
without surfacing an error**, fortfem_api_spaces.f90.

Reproduce one file at a time over `compile_frontend_from_string`,
checking `res%parse_ok` rather than the exit status. A file can fail to
parse without crashing, and counting crashes alone reports these as
fine - which is how they stayed hidden.
