# FortFront roadmap

Snapshot: 2026-08-07. FortFront owns lexing, parsing, semantic resolution,
typed public queries, and diagnostics for the ffc pipeline. It remains
backend-neutral.

## Current truth

The implementation baseline is `39bac6c2` (explicit semantic-context mode
initialization for GCC14, separate module-procedure dummy resolution, implicit
`DIMENSION` dummy preservation, the #2993 `IMPLICIT NONE`
undeclared-reference pass, full-line continuation comments, fixed-form
comment normalization, and the #2255 follow-up). The #2993, #2996,
nested-binding, implicit-DIMENSION, and separate-module-dummy focused oracles
are green locally. The current local GNU lane is green: 1,545 static modules,
381 build targets, 378 derivative targets, 483/483 tests, and clean lint. The
Windows lane and remote aggregate remain open.

Merge CI is [run 31138513938](https://github.com/lazy-fortran/fortfront/actions/runs/31138513938)
for this head and remains in progress; the local GNU gate is not remote-green
evidence.

The semantic context constructor now assigns its input-mode and strict
`IMPLICIT NONE` policy explicitly. This is required for GCC14, where relying
on default initialization of an `INTENT(OUT)` derived context left the strict
gate disabled. The `test_issue_2993_implicit_none_diagnostics` oracle passes
with both local GCC16 and remote GCC14.2 after the explicit initialization.

`test_module_distribution` was previously parallel-fragile because it cleans
shared Makefile artifacts; isolate its outputs before treating the full suite as a
parallel gate. The last-known Windows failures are
`test_compiler_facing_queries`, `test_reject_bind_02_diagnostics`,
`test_reject_placement_01_diagnostics`, `test_reject_value_scope_01_diagnostics`,
`test_all_examples_slow`, and `test_elemental_validation`. Keep them visible;
do not hide them with platform-specific expected output.

The current source-discovery audit found no remaining FortFront self-source
parse/semantic drops. GNU and NVIDIA `nvfortran` 26.5 cold builds cover the
381-target lane; the downstream FortAD nvfortran gate still needs a fresh run
against this revision and is not evidence of a FortFront failure until then.

The canonical downstream plan and current corpus counts live in the
[ffc roadmap](https://github.com/lazy-fortran/ffc/blob/main/ROADMAP.md).
FortFront does not duplicate its parity dashboard.

The nested character-array substring parser contract is now covered locally:
`c(2)(1:3)` retains the complete `c(2)` designator as the `array_slice_node`
base, including when the slice appears on the left-hand side, right-hand side,
or in an actual argument. This is a downstream ffc requirement (ffc #669), not
a source-spelling workaround: the parser must never orphan the range node or
flatten the designator back to `c`.

The same postfix parser keeps a component-access base for
`object%binding(args)` while leaving an ordinary `array(i)` base-free. The
compiled `test_type_bound_call_base_oracle` transforms and runs a complete
type-bound program; downstream FortAD's compiled `test_type_bound_oracle`
checks the receiver is still available for derivative inlining.

The fixed-form file path translates column-1 `C`, `c`, and `*` comments to
free-form `!` comments before lexing. The compiled
`test_fixed_form_comment_oracle` covers a `.f` program with no continuation
line, the case that previously tokenized `C` as code. The normalizer is also
available through the compatibility facade for source-aware downstream file
drivers. This is one source-form fix, not a claim of complete fixed-form or
preprocessor support.

## Required public architecture

ffc consumes one immutable typed program snapshot through public FortFront
APIs. The supported boundary provides:

- stable node and declaration-binding identities, including declaration
  entity, owning scope, and direct/host/use/associate relationship.
- fully represented declarations, expressions, procedure signatures, generic
  specifics, result types, shapes/ranks, type parameters, and source spans.
- structured diagnostics with category, location, related locations, and
  standard/dialect context.
- explicit ownership and lifetime for AST/semantic snapshots so downstream
  tools share them without shallow or repeated deep copies.
- versioned public query behavior, with no consumer importing private arena
  layout or recovering missing facts from source spelling.

A semantic fact needed by more than one consumer is implemented here once. A
backend policy, descriptor layout, runtime ABI, or LIRIC decision stays in
ffc. Breaking public changes are allowed when the old contract is unsound:
add the typed replacement, migrate all in-tree consumers, then delete the old
query in the same set of linked commits. Do not leave a permanent fallback.

## Execution order

1. Restore Windows and aggregate CI. Reproduce each failure on the same
   compiler/platform and preserve its exact signature.
2. Generate a fresh accepted/rejected corpus baseline. Record raw outcomes,
   not prose counts, and keep invalid and valid-side gates paired.
3. The missing semantic rejection in
   [#2993](https://github.com/lazy-fortran/fortfront/issues/2993) is landed in
   `4bd83caf`: the arena-wide structural `IMPLICIT NONE` reference pass covers
   declaration, host/use/associate, procedure, construct-entity, component,
   keyword, NAMELIST, and SELECT TYPE bindings. It runs when callers select
   standard analysis while preserving the Lazy transformation inference
   boundary (including Lazy sources that contain `IMPLICIT NONE`). The focused
   API oracle covers that lazy boundary and explicit `INPUT_MODE_STANDARD`, and
   the accepted neighbor is independently syntax-checked with GNU Fortran.
   Next close the accepted side of [#2897](https://github.com/lazy-fortran/fortfront/issues/2897) and
   the lost or malformed AST evidence in #2986/#2987 without over-rejecting
   valid code.
4. Preserve legacy declaration forms as typed AST facts. The implicit
   `DIMENSION` dummy path is landed in `e5e8157b` with a compiled source-vs-
   transformed behavior oracle; extend this pattern to the remaining fixed-
   form and declaration-shape cases before reclassifying corpus rows.
5. Resolve separate module-procedure dummies by interface identity. The
   implementation is landed in `8d3e5a8a`; the three-test focused oracle
   (`test_compiler_scope_resolution`, `test_compiler_facing_queries`, and
   `test_f2008_submodule_constructs`) passes on the current head. Preserve the
   rule that the resolver consumes the parent module interface rather than
   guessing from source spelling.
6. Complete binding identity across nested ASSOCIATE in
   [#2975](https://github.com/lazy-fortran/fortfront/issues/2975). The selector
   binding correction and exact declaration-identity oracle are now landed;
   this remains the frontend prerequisite for ffc #584 and removal of
   text-name lookup.
7. Fix parser representation defects #2973 and the remaining explicit-
   interface representation work #2970. Then rebaseline stale umbrellas
   #2883/#2924/#2951 and close or split them by live signature.
8. Establish Lazy result/specialization identity in #2980/#2994 before ffc
   serializes or emits it.
9. Fix the full-line-comment-inside-continuation lexer defect in
   [#2996](https://github.com/lazy-fortran/fortfront/issues/2996) with its
   parser reproduction, token/position oracle, gfortran accepted neighbor, and
   a malformed-continuation rejection control. The implementation keeps the
   lexer continuation state alive across comment/blank trivia while preserving
   character-literal state separately; the focused tests pass locally.
10. Keep the nested character-array substring AST contract green for downstream
   ffc #669 and run its read/write/overlap/actual-argument differential oracle.
11. Run downstream ffc binding, rejection, module-consumer, and Lazy ABI gates
   against the exact FortFront revision before release.

Parser, semantic, query, and portability work may proceed in isolated
worktrees. Changes to the same public query family merge as passing commits in
dependency order. Full builds on a constrained host run one at a time.

### #2996 evidence tranche

The lexer normalization state machine now suppresses physical newlines from
full-line comment/blank trivia encountered after a trailing continuation
ampersand while an expression remains parenthesized; it preserves the logical
newline needed by a continued inline `IF` body. Parenthesis depth is tracked
from operator tokens, so character-literal state remains independent and `!`
inside literals is never treated as a comment marker.
The lexer edge-case test checks raw token positions and literal/comment
separation. The parser regression checks `parse_ok` and `semantic_ok` for both
the commented and plain `SELECT CASE` forms, then checks the hand-authored AST
contains both case values. A missing continuation marker is rejected. On an
idle GNU host, both FortFront-generated and direct-gfortran executables print
the byte-identical `PASS` output. The existing inline-IF comment-continuation
regression also passes. Remote CI and the full corpus gate remain required
before closing the issue.

## Open issue map

All open issues as of the snapshot are assigned below.

| Workstream | Issues |
| --- | --- |
| rejection and accepted-side correctness | [#2883](https://github.com/lazy-fortran/fortfront/issues/2883), [#2897](https://github.com/lazy-fortran/fortfront/issues/2897), [#2924](https://github.com/lazy-fortran/fortfront/issues/2924), [#2951](https://github.com/lazy-fortran/fortfront/issues/2951), [#2970](https://github.com/lazy-fortran/fortfront/issues/2970), [#2986](https://github.com/lazy-fortran/fortfront/issues/2986), and [#2987](https://github.com/lazy-fortran/fortfront/issues/2987). #2993 is landed in `4bd83caf`. |
| parser and semantic identity | [#2973](https://github.com/lazy-fortran/fortfront/issues/2973), [#2975](https://github.com/lazy-fortran/fortfront/issues/2975), [#2980](https://github.com/lazy-fortran/fortfront/issues/2980), [#2994](https://github.com/lazy-fortran/fortfront/issues/2994) |
| deferred syntax | [#2976](https://github.com/lazy-fortran/fortfront/issues/2976) |
| lexer continuation correctness | [#2996](https://github.com/lazy-fortran/fortfront/issues/2996) |

#2924's rejection gate and much of #2883/#2951 have partial implementations
on main. Re-run their named invalid cases and valid corpus neighbors, keep only
live signatures as child issues, then close stale umbrellas. Closed silent-
source-drop issues #2966/#2967/#2972/#2974/#2977 are not active blockers.

Fortran Synthesis #2976 starts only after standard #756 is accepted. It does
not block standard-Fortran convergence.

## Minimum verification

Tests need an independent behavioral oracle:

- lexer/parser: compare the public tree/query result to a hand-authored
  expected structure and require a source round-trip or compiler acceptance
  where appropriate.
- semantic acceptance: compile and run the valid neighbor with an independent
  compiler.
- rejection: require category/location for the invalid case and acceptance of
  a minimally different valid case.
- #2993 (landed): retain `test_issue_2993_implicit_none_diagnostics` (lazy
  auto-detection and explicit standard mode) and the bounded
  `scripts/corpus_rejection_gate.sh` shard as merge-train regression gates;
  every new accepted-to-rejected delta must be fixed or named as pre-existing
  baseline drift.
- binding/query changes: query exact declaration identities in nested scopes,
  then compile/link/run an ffc consumer that would fail under name lookup.
- module and Lazy ABI changes: separate producer/consumer compilation and
  runtime behavior, not artifact existence.
- portability fixes: a cold build and focused behavioral test on every touched
  supported compiler lane.

Per edit, run the focused oracle and affected query tests. Before commit, run
the maintained `fo` pipeline once from a clean dependency state. At the merge
train boundary, run the accepted/rejected corpus shard and the downstream ffc
cluster. Documentation for a public-query or ownership change lands with the
code.

GNU Fortran, NVIDIA `nvfortran`, Intel LLVM `ifx`, and LLVM Flang are distinct
supported lanes. Legacy `ifort` is not. A compiler-specific workaround cannot
weaken the public semantic contract.
