# Repository Guidelines

## Project Structure & Module Organization
- `src/`: core Fortran sources (lexer, parser, semantic, AST, codegen).
- `app/`: CLI/app entry points.
- `test/`: auto-discovered fpm tests, grouped by domain (e.g., `parser/`, `semantic/`).
- `docs/` and `DOCS/`: user and internal notes.
- `examples/`: sample tools and usage.
- `scripts/`: helper scripts; no secrets.
- Root build files: `fpm.toml`, `Makefile`, `CMakeLists.txt`.
- Generated artifacts: `build/`, `fortfront_modules/`, `libfortfront.a` (do not commit).

## Build, Test, and Development Commands
- `make` or `make build`: fpm build with preprocessing flags.
- `make test`: run the full test suite (fpm auto-tests).
- `make coverage`: run tests with gcov/lcov and generate HTML in `coverage_html/`.
- `make clean`: remove build and generated artifacts.
- Alternative CMake: `cmake -S . -B build && cmake --build build` (no tests).
- Install library: `make install PREFIX=/usr/local` (uses `pkg-config: fortfront`).

## Coding Style & Naming Conventions
- Fortran free-form, no implicit typing/externals (enforced in `fpm.toml`).
- Indentation: 4 spaces; line width target 88 columns.
- Prefer small procedures (<50 lines); avoid commented-out code and stubs.
- Derived types: `typename_t`; modules/files use clear, domain-based names (e.g., `frontend_parsing.f90`).

## Testing Guidelines
- Framework: fpm auto-tests under `test/` (one executable per subdir/file).
- Add new tests beside related code domain (e.g., `test/parser/…`).
- Run locally: `make test`; generate coverage with `make coverage`.
- Keep or improve coverage; add regression tests for every bugfix.

## Commit & Pull Request Guidelines
- Conventional Commits (imperative, <72 chars), e.g., `feat(parser): add array slices`.
- Scope a commit to one topic; do not commit binaries or `build/` outputs.
- Before pushing: `make test` must pass locally; include relevant logs in the PR.
- PRs: clear description, linked issue(s), rationale, commands to reproduce, and any screenshots/logs.

## Security & Configuration Tips
- No secrets in sources or scripts. Pin external deps via `fpm.toml`.
- Clean stale `.mod` files if module resolution issues occur: `make clean`.
