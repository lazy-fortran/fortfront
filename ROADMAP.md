# FortFront roadmap

Snapshot: 2026-08-06. FortFront owns lexing, parsing, semantic resolution,
typed public queries, and diagnostics for the ffc pipeline. It remains
backend-neutral.

## Current truth

The implementation baseline is `ac02b4d0` (component-access query facade). Its
latest check is
[run 31127135731](https://github.com/lazy-fortran/fortfront/actions/runs/31127135731):
Windows and the aggregate gate failed, while the Ubuntu lane was cancelled.
The previous checked ancestor `e84ac97b` in
[run 31110432510](https://github.com/lazy-fortran/fortfront/actions/runs/31110432510)
had the same Windows/aggregate defect. The intervening main commits
`c40ce77e` (continued character literals), `11da10a4` (portable backslashes),
and `ac02b4d0` remain locally evidence-backed but not remote-green.

The current local GNU lane builds 381 targets and 379 test programs across 484
tests. `test_module_distribution` is parallel-fragile because it cleans shared
Makefile artifacts; isolate its outputs before treating the full suite as a
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
3. Close the missing semantic rejection in
   [#2993](https://github.com/lazy-fortran/fortfront/issues/2993), the accepted
   side of [#2897](https://github.com/lazy-fortran/fortfront/issues/2897), and
   the lost or malformed AST evidence in #2986/#2987 without over-rejecting
   valid code.
4. Complete binding identity across nested ASSOCIATE in
   [#2975](https://github.com/lazy-fortran/fortfront/issues/2975). This is the
   frontend prerequisite for ffc #584 and removal of text-name lookup.
5. Fix parser representation defects #2973 and the remaining explicit-
   interface representation work #2970. Then rebaseline stale umbrellas
   #2883/#2924/#2951 and close or split them by live signature.
6. Establish Lazy result/specialization identity in #2980/#2994 before ffc
   serializes or emits it.
7. Fix the full-line-comment-inside-continuation lexer defect in
   [#2996](https://github.com/lazy-fortran/fortfront/issues/2996) with its
   parser reproduction and gfortran accepted neighbor.
8. Run downstream ffc binding, rejection, module-consumer, and Lazy ABI gates
   against the exact FortFront revision before release.

Parser, semantic, query, and portability work may proceed in isolated
worktrees. Changes to the same public query family merge as passing commits in
dependency order. Full builds on a constrained host run one at a time.

## Open issue map

All open issues as of the snapshot are assigned below.

| Workstream | Issues |
| --- | --- |
| rejection and accepted-side correctness | [#2883](https://github.com/lazy-fortran/fortfront/issues/2883), [#2897](https://github.com/lazy-fortran/fortfront/issues/2897), [#2924](https://github.com/lazy-fortran/fortfront/issues/2924), [#2951](https://github.com/lazy-fortran/fortfront/issues/2951), [#2970](https://github.com/lazy-fortran/fortfront/issues/2970), [#2986](https://github.com/lazy-fortran/fortfront/issues/2986), [#2987](https://github.com/lazy-fortran/fortfront/issues/2987), [#2993](https://github.com/lazy-fortran/fortfront/issues/2993) |
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
