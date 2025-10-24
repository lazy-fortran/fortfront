# CLAUDE.md - fortfront Project Rules

## Examples & Tests Organization

### CRITICAL: Zero Duplication Policy
**ONE canonical example, many references. NO DUPLICATION EVER.**

### Directory Structure
```
examples/
├── f90/          # Standard Fortran examples
│   └── *.f90     # Descriptive names (feature_*.f90, demo_*.f90)
└── lf/           # Lazy Fortran examples
    └── *.lf      # Descriptive names (feature_*.lf, demo_*.lf)

test/
├── snapshots/
│   └── cases/    # MUST be empty of .lf files (all moved to examples/)
└── *.f90         # Test files that REFERENCE examples/, never inline duplicate content
```

### Rules

1. **Examples are canonical sources**
   - `examples/` contains THE definitive example code
   - Examples demonstrate features, edge cases, and issue resolutions
   - Named descriptively: `generic_functions.lf`, `array_syntax.lf`, NOT `test_*.lf`
   - Issue demonstrations: `issue_NNNN_description.lf` → rename to `feature_description.lf`

2. **Tests reference examples**
   - Tests in `test/` MUST NOT duplicate example content inline
   - Tests should read from `examples/` files when testing parsing/transformation
   - If a test generates Lazy Fortran code, extract it to `examples/` and reference it
   - This prevents drift: examples and tests stay synchronized automatically

3. **Deduplication enforcement**
   - Before ANY commit touching examples/ or test/:
     - Run deduplication audit (see issue #1867)
     - Verify zero .lf files remain in `test/snapshots/cases/`
     - Verify no string literals in tests duplicate example file content
   - CI MUST validate: no duplicates exist

4. **Adding new examples**
   - Place in appropriate subdirectory: `examples/f90/` or `examples/lf/`
   - Use descriptive name reflecting what it demonstrates
   - Update tests to reference the new example file
   - Never inline the same code in both places

5. **Adding new tests**
   - If test needs Lazy Fortran input: create example file first, reference it
   - If test needs standard Fortran: create example file first, reference it
   - Never duplicate existing examples/ content

### Rationale
- **Single source of truth**: Examples are documentation AND test inputs
- **No drift**: Tests always use current example code
- **Clear purpose**: examples/ = documentation, test/ = validation logic
- **Maintainability**: Change example once, all tests automatically updated
- **Repository hygiene**: Obvious what each directory contains

### Migration Status
See issue #1867 for ongoing reorganization work.

## Build & Test
- Build tool: `fpm` (Fortran Package Manager)
- Test command: `fpm test`
- Build command: `fpm build`
- Formatting: `fprettify` with 88-column limit, 4-space indent
- All files end with newline

## Fortran Standards
- Modern Fortran (2018+)
- Use `allocatable`, avoid pointers unless required
- All procedures have explicit `intent(in|out|inout)`
- Mark `pure`/`elemental` where appropriate
- Derived types named `<name>_t`
- Use `use <module>, only:` statements

## Git Workflow
- SSH only, no HTTPS
- Stage files explicitly: `git add path/to/file`, NEVER `git add .` or `git add -A`
- No emojis in commits, PRs, or issues
- CI must pass before merge
- Run `fpm test` locally before creating/updating PRs

## GitHub CLI Usage
- List issues: `gh issue list --state open --limit 500`
- List PRs: `gh pr list --state open --limit 500`
- Edit issue body: `gh issue edit <number> --body-file <file.md>`
- Create PR: `gh pr create --title "<title>" --body-file <file.md> --base main --head <branch>`
- Check CI: `gh pr checks <number> --watch`

## Code Quality
- Modules <500 lines (hard limit 1000)
- Functions <50 lines (hard limit 100)
- No stubs, placeholders, or commented-out code
- No hardcoded secrets/keys
- Remove dead code immediately
- Self-documenting code; comments for non-obvious intent only

## Documentation
- Keep in `docs/` directory
- No random markdown files in working directory
- Update docs when behavior changes
- Examples in `examples/`, not inline in docs

## Licensing
- Research-first: copy ideas, not lines
- Verify licenses: prefer MIT/BSD/Apache-2.0
- Preserve notices when required
