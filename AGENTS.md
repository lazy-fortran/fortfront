# Repository Guidelines (Agent Notes)

## Project Structure & Module Organization
- `src/`: core Fortran sources (lexer, parser, semantic, AST, codegen).
- `app/`: CLI/app entry points.
- `test/`: auto-discovered fpm tests, grouped by domain (e.g., `parser/`, `semantic/`).
- `docs/` and `DOCS/`: user and internal notes.
- `examples/`: sample tools and usage.
- Root build files: `fpm.toml`, `Makefile`.
- Generated artifacts: `build/` (do not commit).

## Build, Test, and Development Commands
- Preferred: use only `make` and `fpm`.
- Build: `make` (equivalent to `fpm build`).
- Test: `make test` (equivalent to `fpm test`).
- Clean: `make clean` (equivalent to `fpm clean --all`).
- Do not pass `-j` to `make` or `fpm`. fpm manages parallelism automatically.
- Do not add custom compiler flags (e.g., `-cpp`, `-fmax-stack-var-size`). Use defaults.

## Coding Style & Naming Conventions
- Fortran free-form, no implicit typing/externals (enforced in `fpm.toml`).
- Indentation: 4 spaces; line width target 90 columns. Use `fprettify -c .fprettify`
- Prefer small procedures (<50 lines); avoid commented-out code and stubs.
- Derived types: `typename_t`; modules/files use clear, domain-based names (e.g., `frontend_parsing.f90`).

## Testing Guidelines
- Framework: fpm auto-tests under `test/` (one executable per subdir/file).
- Add new tests beside related code domain (e.g., `test/parser/…`).
- Run locally with `make test` only (no wrappers, no extra flags).
- Add regression tests for every bugfix.

## Commit & Pull Request Guidelines
- Conventional Commits (imperative, <72 chars), e.g., `feat(parser): add array slices`.
- Scope a commit to one topic; do not commit binaries or `build/` outputs.
- Before pushing: `make test` must pass locally.
- PRs: clear description, linked issue(s), rationale, and minimal commands to reproduce.

## CI & Platform Notes
- CI runs Linux and Windows. Windows tests include CLI piping (stdin) scenarios.
- No custom build/test scripts in CI; just `make` and `make test`.
- Avoid any charged terminology in code or logs; keep messages neutral and technical.

## Security & Configuration Tips
- No secrets in sources. Pin external deps via `fpm.toml`.
- If module resolution issues occur locally, run `make clean`.
