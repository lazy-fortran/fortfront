# fortfront Realistic Development Roadmap

## Current State (Reality)

**Working**: Basic lexer and parser for simple Fortran
**Broken**: Build system (#712), various test failures
**Problems**: Oversized files/functions, incomplete error handling

## Phase 1: Foundation Repair (Current Sprint)

**Goal**: Get basic system working again

1. **Fix Build System** (#712)
   - Resolve missing `tokenize_with_options` function
   - Ensure `./build.sh` works without errors
   - Single command compilation

2. **Repository Cleanup** (✅ Mostly Complete)
   - ✅ Removed obsolete docs (#722)
   - ✅ Cleaned debug files (#723)
   - ✅ Simplified architecture (#725)

3. **Core Functionality**
   - Verify basic parsing works
   - Test simple code generation
   - Document what actually works

**Timeline**: 1-2 sprints
**Success**: System builds and parses "Hello World"

## Phase 2: Size Compliance (Next 2-3 Sprints)

**Goal**: Meet code quality constraints

1. **Split Oversized Files** (12 files >1000 lines)
   - ast_factory.f90 (1911 lines) → Split to <500 each
   - parser_declarations.f90 (1460 lines) → Modularize
   - parser_expressions.f90 (1162 lines) → Separate concerns

2. **Refactor Large Functions** (35 functions >100 lines)
   - Break into smaller, focused functions
   - Target <50 lines per function
   - Improve readability

3. **Remove error_stop Usage** (1,386 violations)
   - Replace with result_t error handling
   - Gradual migration, module by module

**Timeline**: 2-3 sprints  
**Success**: No files >1000 lines, no functions >100 lines

## Phase 3: Parser Completeness (Sprints 4-6)

**Goal**: Handle common Fortran constructs reliably

1. **Fix Critical Bugs**
   - Semicolon statement parsing (#492)
   - Operator precedence (#493)
   - Multi-variable declarations (#652)

2. **Core Fortran Support**
   - DO loops with expressions
   - IF/ELSE constructs
   - Basic I/O statements
   - Array literals

3. **Test Coverage**
   - One test per supported construct
   - Regression tests for fixed bugs
   - Performance benchmarks

**Timeline**: 2-3 sprints
**Success**: Parse 80% of common Fortran patterns

## Phase 4: Production Ready (Sprints 7-9)

**Goal**: Stable, usable library

1. **Documentation**
   - API documentation
   - Usage examples
   - Migration guide from error_stop

2. **Integration**
   - Static library build working
   - FPM package fully functional
   - CI/CD pipeline stable

3. **Performance**
   - Parsing <1s for 1000-line files
   - Memory usage reasonable
   - No memory leaks

**Timeline**: 2-3 sprints
**Success**: External projects can use fortfront reliably

## What We Will NOT Do

**Never Attempt These** (Team lacks capability):

- ❌ Concrete Syntax Trees (CST)
- ❌ Arena memory management
- ❌ Type inference systems
- ❌ Constraint solving
- ❌ Advanced optimizations
- ❌ Compiler backends
- ❌ IDE protocol implementations

## Success Metrics

### Sprint Success
- Build works without errors
- Tests pass without hanging
- Can parse basic Fortran programs
- Generates valid Fortran output

### Project Success
- Used by 3+ external projects
- <5 critical bugs per release
- Documentation matches implementation
- Team can maintain without confusion

## Resource Allocation

**Focus Areas** (% of effort):
- 40% - Bug fixes and stability
- 30% - Size/quality compliance  
- 20% - Test coverage
- 10% - Documentation

**Avoid**:
- 0% - Advanced type theory
- 0% - Memory management systems
- 0% - Theoretical architecture

---

*This roadmap represents achievable goals based on honest team capability assessment. Attempting anything marked with ❌ will cause project failure.*