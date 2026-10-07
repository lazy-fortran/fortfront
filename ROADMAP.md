# FortFront goals

Provide correct source interpretation, semantic analysis, public compiler facts
and diagnostics for standard Fortran and accepted Lazy Fortran modes. Remain a
reusable frontend for FFC and other consumers.

Apply [goals and architectural freedom](https://github.com/lazy-fortran/fo/blob/main/doc/GOAL_DRIVEN_DEVELOPMENT.md).
Agents choose and revise internal structure when evidence requires. Existing
public semantic contracts remain authoritative; a module split or AST layout
sketched in an earlier issue is not a mandatory implementation.

## Current goals

2026-10-07 cleanup: the broad `fortfront`, `ast_nodes_control`, and
`ast_factory` facades, the `ast_arena_compat` inheritance layer, unused
parser/type wrappers and token aliases are removed. Indexed AST storage now
lives in `ast_arena_modern`; its live size field is `entry_count`. FortFront
declares `examples/` as a resident test input. All FPM test targets compiled;
focused native tests passed. The exact Fo Gremlin generation
`e7f62b95e95a5c4e7e2ee78fa76c73ee858913cec2613913eb513ece134d9045`
passed 13/13 focused cases with zero current failures. FFC `c9cfdfe` passed
three focused compiler consumers against the combined FortFront candidate in
generation `f6e57e6851e4441e1e665f34a3ba2866014ef53f6ac02388917fe2946e97a50f`.
These receipts do not claim full test-suite execution.

- Repair current false acceptance/rejection, dropped source meaning and memory/
  arithmetic defects using independent public/consumer examples.
- Give FFC enough public information to implement the required standard without
  private-arena workarounds or redundant frontend interpretation.
- Complete required Fortran 2023 declarations, expressions, I/O and intrinsic
  semantics and coarray information.
- Substantially reduce maintained implementation/test/documentation volume as
  part of [Fo #205](https://github.com/lazy-fortran/fo/issues/205), preserving
  useful features and independently correct downstream behavior.

## Active goal owners

| Outcome | Issues |
| --- | --- |
| Precise declaration/expression rejection and valid neighbors | #2883, #2897, #2970, #3021 |
| Restore valid corpus acceptance | #2951 |
| Continuation and literal-substring meaning | #2996, #3018 |
| Memory-safe repeated use and stable hashing | #3019 |
| Truthful input inspection | #3020 |
| F2023 source/type/rank/bounds/enum declarations | #3022–#3028 |
| Grouped subscripts and conditional expressions/arguments | #3029–#3031 |
| SIMPLE, REDUCE and I/O policy | #3032–#3034 |
| Intrinsic identities/signatures and coarray facts | #3035, #3036 |
| Accepted optional Synthesis | #2976, standard#756 |

Earlier tranches include delivered public queries and diagnostics; reuse their
current behavior. Recheck historical observations before claiming a live defect.
Issues and contracts define the required outcomes, not a compulsory edit-file
list, representation or extraction sequence.

## Verification and boundaries

Public clients and actual FFC consumers establish source meaning and results.
Keep independent valid/invalid neighbors, exact source locations, binding
identity, type/kind/rank/corank, lifetime and compatibility semantics where
required. A syntax-only success cannot establish correct consumer behavior.

The [FFC plan](https://github.com/lazy-fortran/ffc/blob/main/PLAN.md) owns the full
compiler destination, including all ISO parallel features. Work begins when its
actual consumer dependencies are satisfied; unrelated Fo cleanup and remote CI
are not prerequisites. Use resident Gremlin and focused checks with exact
identities; broad platform/corpus audits remain milestone/background evidence.

Preserve current scientific/consumer contracts when redesigning a provider.
Version and verify an intentionally changed public contract with its consumers.
Reduce total duplication instead of merely moving it into more modules.

Historical status, query details and platform observations remain at
[the pre-revision roadmap](https://github.com/lazy-fortran/fortfront/blob/5bc3fd5a2a494d142864ce99048e2b859f59060d/ROADMAP.md).
Those dated observations are not claims of current full platform green.
