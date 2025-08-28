# fortfront Design - REALITY-BASED ARCHITECTURE

## Mission: Simple Fortran Frontend Library

**WHAT IT IS**: A basic Fortran parser and code generator library  
**WHAT IT ISN'T**: A compiler, optimizer, or advanced type system  

## Working Components (What Actually Functions)

1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source
2. **Parser** (`src/parser/`) - Builds simple AST from tokens
3. **Codegen** (`src/codegen/`) - Generates Fortran output
4. **Tests** (`test/`) - Basic validation suite

## Simple Pipeline

```
Source Code → Lexer → Parser → AST → Codegen → Fortran
```

That's it. No CST. No arenas. No advanced type theory.

## Build Commands 

```bash
# Build the library
fpm build --flag "-cpp -fmax-stack-var-size=524288"

# Run tests
fpm test --flag "-cpp -fmax-stack-var-size=524288"
```

## Current Reality

**WORKING**: Basic parsing and code generation for simple Fortran  
**BROKEN**: Build system (Issue #712), various test failures  
**PROBLEMS**: 35+ functions over 100 lines, 12+ files over 1000 lines  

## Achievable Goals (Next 2-3 Sprints)

1. **Fix Build System** - Get `./build.sh` working again
2. **Clean Repository** - Remove debug files and obsolete docs
3. **Basic Parsing** - Handle common Fortran constructs reliably
4. **Simple Codegen** - Generate correct Fortran for parsed input
5. **Size Compliance** - Refactor oversized files and functions

## What We DON'T Do (Team Cannot Implement)

- ❌ Concrete Syntax Trees (CST)
- ❌ Arena memory management 
- ❌ Hindley-Milner type inference
- ❌ Advanced constraint solving
- ❌ Generational garbage collection
- ❌ Complex type systems
- ❌ Compiler optimizations
- ❌ IDE integrations

## Development Principles

1. **KISS** - Keep it stupidly simple
2. **Working Code** - Broken features get deleted, not fixed forever
3. **Size Limits** - Files <1000 lines, functions <100 lines
4. **No Abstractions** - Direct, obvious code over clever patterns
5. **Test Coverage** - Every feature has a working test

## File Organization

```
fortfront/
├── src/
│   ├── lexer/       # Token generation
│   ├── parser/      # AST construction  
│   ├── codegen/     # Fortran output
│   └── utils/       # Shared utilities
├── test/            # Test suite
├── build.sh         # Simple build script (needs fix)
└── fpm.toml         # Package configuration
```

## Success Criteria

✅ **Sprint Success**: Basic Fortran programs parse and generate correctly  
✅ **Build Success**: One command builds everything without errors  
✅ **Test Success**: Core tests pass without hanging or crashing  
✅ **Code Success**: No functions >100 lines, no files >1000 lines  

## Team Capability Assessment

**CAN DO**: Basic parsing, simple AST, direct code generation  
**CANNOT DO**: Advanced type theory, memory management, optimization  
**FOCUS**: Get basics working before adding any complexity  

---

*This design reflects what the team can actually build and maintain.*