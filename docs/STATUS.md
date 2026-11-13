# Support and limitations

FortFront is an experimental Fortran frontend. It parses and analyzes source,
provides typed queries, and transforms Lazy Fortran into standard Fortran text.
It does not emit object files or executables. Acceptance by FortFront does not
establish complete conformance to any edition of the Fortran standard.

## Supported paths

The public modules cover tokenization, arena-based syntax inspection, source
locations and trivia, semantic analysis, transformation and compiler-facing
queries. The [library guide](guides/LIBRARY_USAGE.md) describes the contracts
and explicit refusal flags for bounded semantic queries.

Regression tests cover continuation comments, constant-folded character-literal
substrings, and substrings of character literals with runtime bounds. Compiler
queries distinguish character substrings from array sections and preserve the
runtime bounds. The `debug_ast FILE` application inspects its supplied source
and reports input errors.

Evidence for these paths is in the native tests, including
[continuation comments](../test/parser/test_issue_2996_continuation_comment.f90),
[literal substrings](../test/integration/issue_tests/test_issue_3018_literal_substring.f90)
and [file inspection](../test/cli/test_debug_ast_input.f90). The
[conformance guide](CONFORMANCE.md) describes broader optional checks; it does
not claim that every program in an external suite passes.

## Unsupported and incomplete paths

Fortran 2023 support is incomplete. Conditional expressions and conditional
arguments are diagnosed as unsupported; their lazy branch evaluation is not
implemented. The [conditional diagnostic regression](../test/api/test_issue_3030_conditional_diagnostic.f90)
checks that these expressions cannot silently become a different result.
Newer declarations, intrinsics and coarray information remain areas of active
work. Some nested internal procedures and legacy constructs are unsupported.

Cross-file inference and specialization are not implemented. The C interface
provides a small validation/status bridge rather than the Fortran syntax and
semantic query API. Semantic analysis uses process-wide state, so concurrent
library calls within one process are not supported; use separate processes
for parallel file processing.

A consumer must check diagnostics and query refusal or unresolved flags before
using a result. Owned-array exact dynamic identity additionally requires a
matching concrete acquisition; a `CLASS IS` guard alone can match extensions.
A parsed node alone does not prove that its semantics are
supported by either FortFront or a downstream compiler.
