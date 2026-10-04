# FortFront agent rules

## Destination and first reads

FortFront parses and analyzes standard Fortran and Lazy Fortran, exposes useful
public typed queries to consumers such as FFC, and transforms Lazy Fortran into
standard Fortran. Read [ROADMAP.md](ROADMAP.md) for current goals and the owning
issue for its acceptance evidence. Existing accepted language and public API
contracts remain authoritative. Grammar proposals are not accepted semantics.

General user/workspace rules continue to apply. AGENTS.md is a symlink to this
file; maintain one instruction source.

## Goals and architectural freedom

Issues define observable outcomes, independent acceptance and essential
compatibility. Make architectural decisions as soon as required and as late as
possible. Choose the smallest adequate implementation; internal representations,
module layout, traversal patterns and extraction sequences may evolve when an
actual need appears. Do not require speculative architecture before useful work.

Substantially reduce maintained code through
[Fo #205](https://github.com/lazy-fortran/fo/issues/205), counting the whole
affected stack. Remove duplicated responsibility, obsolete code, weak tests and
repetitive documentation while preserving supported behavior and useful failure
detection. Arbitrary module/procedure limits and one README per directory are
not goals. [Shared development principles](https://github.com/lazy-fortran/fo/blob/main/doc/GOAL_DRIVEN_DEVELOPMENT.md)
apply alongside existing ownership and escalation rules.

## Correctness boundaries

- Preserve standard Fortran semantics and separately accepted Lazy Fortran
  inference/transformation behavior. Invalid input needs useful diagnostics.
- Public compiler queries remain backend-neutral and support actual FFC needs.
  Repair missing producer behavior here and recheck the consumer; private-arena
  consumer workarounds do not replace an upstream repair.
- Respect current arena ownership and node lifetimes. While that representation
  is in use, avoid unsafe copying/deallocation and use its supported accessors.
  Ownership safety is binding; a particular traversal pattern is not permanent.
- Preserve scope and binding identity, type/kind/rank information and supported
  source fidelity through analysis and emission. Representation changes require
  independent behavioral evidence, including affected consumers.
- Fortran logical operators do not short-circuit: guard indexing, association,
  allocation and optional arguments in separate statements.

## Tests and examples

Every support claim needs an independent expected result, invalid neighbors and
important boundaries. Use the public API for library behavior and actual
compile/run checks for emitted programs where appropriate. Source-shape and
patch-conformity checks do not prove semantics.

Reuse canonical examples when several tests exercise the same program; keep
small focused inputs close to their oracle when clearer. Avoid duplicated full
programs and parallel fixture systems. Preserve original public case names and
useful independent observations when consolidating tests. Do not create tests
that enforce prose, file layout or a proposed architecture.

The current examples are under examples/f90 and examples/lf. Current duplication
checks are maintenance tooling, not a semantic oracle; reconcile them when their
rules conflict with a useful simpler representation. Do not claim a check was
removed or replaced without doing that work. Historical details remain at the
[previous instructions](https://github.com/lazy-fortran/fortfront/blob/5bc3fd5a2a494d142864ce99048e2b859f59060d/CLAUDE.md).

## Local development and delivery

Use resident Fo Gremlin and focused affected/reproducer gates through the exact
candidate driver under the workspace Fortran rules. Repair reproducible Fo/Fx
workflow defects in their owner and recheck FortFront. Keep warm caches and
respect the controller's host admission; do not clean builds indiscriminately
or change scientific/configuration flags to hide a failure.

The controller pushes small locally verified increments promptly. Never wait
for GitHub CI while implementation remains available. Broad frozen-version
verification is a milestone/support claim; performance audits are optional.
Record exact revisions, scope and remaining failures without fabricating green.
Update the owning issue and short roadmap when goals move.

## Communication and hosts

Use English for technical communication. Messages, issues, PRs and comments on
Chris's behalf end with Chris&AI on its own final line. Preserve unrelated work
and stage explicit paths. Workers never promote main or publish installations.
Do not touch faepop* or faepcr* without a user request naming the host.
Every response/tool call stays below about 300 lines or 8k tokens.
