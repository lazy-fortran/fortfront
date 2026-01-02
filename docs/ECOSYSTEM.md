# Fortran Ecosystem Architecture

Fortfront is the core frontend library in the Lazy Fortran tool ecosystem.

## Foundation: fortfront

**Static library** (`libfortfront.a`) providing:
- CST/AST split for source fidelity and semantic analysis
- Arena-based memory management
- Hindley-Milner type inference
- Lazy Fortran (.lf) to standard Fortran transformation
- Plugin architecture with stable interfaces

**Dependencies**: Fortran stdlib (`stdlib`) via `fpm.toml`

## Tool Architecture

The ecosystem tools live in separate repositories and build on fortfront.

| Tool | Purpose | Status | Repository |
|------|---------|--------|------------|
| **fluff** | Static analysis and formatting | Available | [lazy-fortran/fluff](https://github.com/lazy-fortran/fluff) |
| **fortnb** | Notebook processing (`.lf` support) | Available | [lazy-fortran/fortnb](https://github.com/lazy-fortran/fortnb) |
| **ffc** | Compiler (LLVM backend) | Available | [lazy-fortran/ffc](https://github.com/lazy-fortran/ffc) |
| **fortcov** | Coverage analysis | Available | [lazy-fortran/fortcov](https://github.com/lazy-fortran/fortcov) |
| **fortrun** | Build/run enhancement service | Available | [lazy-fortran/fortrun](https://github.com/lazy-fortran/fortrun) |
| **fo** | Universal orchestrator (contains all tools) | Planned | TBD |

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

## Dual .lf Compilation Strategy

**Single-file (ffc)**: Direct compilation with local inference only
```bash
ffc main.lf                # Fast, simple, no cross-module analysis
```

**Multi-file (fo, planned)**: Enhanced compilation via fortrun
```bash
fo run main.lf             # Cross-module type inference, smart caching
```

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
- **Unified experience (planned)**: Single orchestrator for common workflows
- **Consistent parsing**: All tools use identical AST/CST
- **Shared semantics**: One frontend for parsing and analysis across tools
