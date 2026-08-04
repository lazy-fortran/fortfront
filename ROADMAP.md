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

## Segfault under gfortran 13.3 parsing a select case inside a function

Reproduces reliably in a container and is what fails two of fortnum's CI
jobs - the two that invoke `fo`, which bundles fortfront. It does not
reproduce on a newer gfortran, which is why it looked like a CI-only
fault for a long time.

    docker run --rm -v <scratchpad>:/w -w /w ubuntu:24.04 bash /w/ci3.sh

That builds `fo` with `--profile debug` and runs
`fo exec gen_enzyme_scalar_wrappers` under gdb. The backtrace:

    #0  0x0000000000000005 in ?? ()
    #1  parser_statement_core_module::handle_control_keyword
    #2  parse_keyword_statement
    #3  parse_basic_statement_core
    ...
    #7  parser_select_constructs_module::parse_case_arm
    #8  parse_select_case
    ...
    #13 parse_function_definition

So: a control keyword inside a `case` arm of a `select case` inside a
function definition, and `handle_control_keyword` transfers to address
5 rather than to a procedure.

What has been ruled out. Every branch of `handle_control_keyword`
guards its callback with `associated()`, and every one of the nine
pointer components of `statement_callbacks_t` is default-initialised to
`null()`, so neither an unguarded call nor an uninitialised component
explains it on its own. Address 5 is not random - it reads like a small
integer used as an address, which points at a `statement_callbacks_t`
whose storage has been overwritten or reinterpreted somewhere along the
chain from `parse_case_arm` down, rather than at a pointer that was
simply never set.

Next: build that translation unit with `-fsanitize=address` inside the
same container, which should name the write.
