# Contributing to FortFront

FortFront accepts fixes to source interpretation, diagnostics, transformation
and public frontend queries. Start with a small reproducer and the result a
caller should observe. Check the [current limits](docs/STATUS.md) before
claiming support for a language feature.

## Build and checks

Use a Fortran compiler, a C compiler and the Fortran Package Manager (fpm).
GNU Fortran is the primary compiler used by the configured Linux and Windows
checks. Python 3, make and a POSIX shell run the maintenance checks.

```sh
fpm build
RUN_SYSTEM_TESTS=1 fpm test
make check-duplication
make check-duplication-gate
make check-doc-links
make check-rejection-gate
make check-root-cleanliness
```

Build before running system tests: they launch the command-line applications.
The optional external conformance suites are described in
[CONFORMANCE.md](docs/CONFORMANCE.md). A skipped external suite is not evidence
that its programs are supported. Hosted checks run in the merge queue.

## Tests

Add a regression that fails before the fix and checks an independent expected
result. Use public APIs for library contracts. For emitted Fortran, compile and
run the result when that establishes the behavior more directly. Include useful
invalid neighbors and unsupported boundaries.

Keep small, focused inputs beside their unit-test oracle. Store complete
end-to-end programs in `examples/f90/` or `examples/lf/` and read those fixtures
from integration tests. Reuse an existing canonical example instead of copying
a complete program into several tests. The duplication check and its negative
control enforce this distinction.

Tests must clean up their temporary files. Keep generated module, object,
archive and test-output files out of the repository root and product diffs.
Do not weaken a check to make a change pass.

## Implementation

Preserve accepted Fortran semantics, Lazy Fortran inference rules, public query
contracts and source locations. Unsupported input must produce a useful
diagnostic rather than silently losing meaning. Compiler consumers should use
`fortfront_compiler`; repair missing producer facts here rather than asking a
consumer to inspect private arena storage.

Respect arena ownership and node lifetimes. Borrowed node pointers must not
outlive their arena. Fortran logical operators do not guarantee short-circuit
evaluation: guard indexing, optional arguments and unallocated or unassociated
objects in separate statements. Concurrent calls in one process are not yet a
supported library contract.

Keep changes bounded, preserve unrelated work and describe both the verified
behavior and any remaining limits. See the [module map](docs/MODULES.md) for
entry points and source directories.
