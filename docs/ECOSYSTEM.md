# Fortran Ecosystem Architecture

Static-linked Fortran toolchain with zero dependencies.

## Foundation: fortfront

**Static library** (`libfortfront.a`) providing:
- CST/AST split for source fidelity and semantic analysis
- Arena-based memory management
- Hindley-Milner type inference
- Lazy Fortran (.lf) to standard Fortran transformation
- Plugin architecture with stable interfaces

**Dependencies**: Zero - completely self-contained

## Tool Architecture

All tools statically link fortfront only:

| Tool | Purpose |
|------|---------|
| **fluff** | Static analysis and formatting |
| **fortnb** | Notebook processing (.lf support) |
| **ffc** | Single-file .lf compiler (LLVM backend) |
| **fortcov** | Coverage analysis |
| **fortrun** | Universal enhancement service (any compiler) |
| **fo** | Universal orchestrator (contains all tools) |

## fo: Universal Orchestrator

Single executable containing all tools, backward compatible with fpm:

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

**Multi-file (fo)**: Enhanced compilation via fortrun
```bash
fo run main.lf             # Cross-module type inference, smart caching
```

## VSCode Integration

Single point of integration via fo:
```json
{
  "fortran.languageServer": "fo lsp",
  "fortran.formatter": "fo format",
  "fortran.linter": "fo analyze"
}
```

## Architecture Benefits

- **Zero dependencies**: No library conflicts or version hell
- **Self-contained**: Single executable includes everything
- **Consistent parsing**: All tools use identical AST/CST
- **Unified experience**: One tool interface for all functionality
