# Hosted checks

Hosted checks run for merge-queue groups. The
[test workflow](ci.yml) builds with GNU Fortran and fpm on Linux and Windows,
runs native tests with system tests enabled, and reports the aggregate `test`
check. Linux also checks test duplication, its negative control and the
example-corpus rejection baseline.

The [repository hygiene workflow](repo-hygiene.yml) runs the shared repository
checks. Workflow files define the current compiler setup and required checks;
this page does not claim that a particular revision has passed them.

Use the commands in [CONTRIBUTING.md](../../CONTRIBUTING.md) to verify a change
locally before it enters the queue.
