# FortFront contributor instructions

Read [CONTRIBUTING.md](CONTRIBUTING.md) before changing code or tests, and
[docs/STATUS.md](docs/STATUS.md) before making support claims. `AGENTS.md` is a
symlink to this file so both entry points use the same instructions.

Preserve accepted language semantics, public query contracts, source locations
and arena lifetimes. Keep unsupported behavior explicit and verify changed
behavior with an independent regression. Never bypass hooks or weaken a test
to obtain a pass. Stage only the paths belonging to the change.
