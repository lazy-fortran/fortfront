# Fortran Ecosystem Architecture

Fortfront is the core frontend library in the Lazy Fortran tool ecosystem.
It is currently a transformer and tooling frontend, not a compiler backend.

## Foundation: fortfront

**Fortran library** providing:
- CST/AST split for source fidelity and semantic analysis
- Arena-based memory management
- Hindley-Milner type inference
- Lazy Fortran (.lf) to standard Fortran transformation
- Tooling APIs for linters, formatters, and experimental compilers

**Dependencies**: Fortran stdlib (`stdlib`) via `fpm.toml`

## Tool Architecture

The ecosystem tools live in separate repositories and build on fortfront.

| Tool | Purpose | Current status | Repository |
|------|---------|----------------|------------|
| **fluff** | Static analysis and formatting | Experimental; built on FortFront; some README-level feature claims are roadmap items | [lazy-fortran/fluff](https://github.com/lazy-fortran/fluff) |
| **ffc** | Compiler driver/backend | Active experimental compiler driver using FortFront and LIRIC for the supported subset | [lazy-fortran/ffc](https://github.com/lazy-fortran/ffc) |
| **fortrun** | Build/run enhancement service | Experimental and currently on hold | [lazy-fortran/fortrun](https://github.com/lazy-fortran/fortrun) |
| **fortnb** | Notebook processing | Experimental and currently on hold | [lazy-fortran/fortnb](https://github.com/lazy-fortran/fortnb) |
| **fortcov** | Coverage analysis | Independent gcov/FPM coverage tool; not part of compiler bootstrap | [lazy-fortran/fortcov](https://github.com/lazy-fortran/fortcov) |
| **standard** | Language-mode specifications and grammar references | Source of truth for the intended LFortran Standard/Infer behavior | [lazy-fortran/standard](https://github.com/lazy-fortran/standard) |

## fo: Universal Orchestrator (planned)

Planned single executable containing all tools, backward compatible with fpm:

```bash
fo new project              # Project initialization
fo build                    # Enhanced build with caching
fo test                     # Testing with coverage integration
fo run main.lf              # Execute with fortrun enhancement
fo analyze                  # fluff static analysis
fo format                   # fluff code formatting
```

## Compiler Direction

The active compiler path keeps FortFront backend-neutral. FortFront owns
lexing, parsing, type inference, AST standardization, and diagnostics.
`ffc` consumes the typed frontend result and lowers the supported subset
through LIRIC. LIRIC stays behind `ffc`.

LIRIC should not be coupled directly into FortFront. FortFront should remain
backend-neutral so `fluff`, `fortrun`, and other tools can keep using the same
frontend.

## VSCode Integration

Single point of integration via fo (planned):
```json
{
  "fortran.languageServer": "fo lsp",
  "fortran.formatter": "fo format",
  "fortran.linter": "fo analyze"
}
```

## Architecture Benefits

- **Minimal dependencies**: Fortfront builds as an `fpm` package with `stdlib`
- **Consistent parsing**: Tools can share one frontend instead of re-parsing
- **Shared semantics**: Type inference and diagnostics are centralized
- **Backend separation**: Native code generation stays in compiler drivers
