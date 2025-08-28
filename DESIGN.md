# fortfront Architecture Design

## 🎯 ESSENTIALS-FIRST DEVELOPMENT - SIMPLIFIED APPROACH

**NEW STRATEGIC CONTEXT**: CLAUDE.md simplified from 275 lines to 35 lines of essentials  
**APPROACH SHIFT**: "GET BASICS WORKING FIRST" vs architectural dreams  
**TEAM FOCUS**: Essential functionality with achievable goals  

**CURRENT GOAL**: Build working foundation with simplified guidance

### REPOSITORY REALITY CHECK 

**DOCUMENTATION BLOAT**: 40+ obsolete files polluting repository (Issue #722)
**DEBUG TRASH**: 80+ scattered debug/test files making repository unprofessional (Issue #723)
**BUILD COMPLEXITY**: Multiple broken build scripts overwhelming team (Issue #724)
**ARCHITECTURE OVERENGINEERING**: DESIGN.md too complex for team capability (Issue #725)
**GUIDANCE OVERWHELM**: 275-line CLAUDE.md paralyzing development

**ESSENTIALS DEFINITION OF DONE** (Simplified Success):
1. Repository is clean and professional
2. Build system is simple with one command that works
3. Documentation matches actual functionality
4. Core functionality works for basic cases
5. Team can develop without complexity overwhelm
6. Realistic architecture that team can implement

**🎯 ESSENTIALS-FIRST PLAN** (Simplified Priorities):

### Priority 1: REPOSITORY CLEANUP (Issues #722-723)
**IMMEDIATE IMPACT**: Clean professional repository
**ESSENTIAL FIX**:
- Remove 40+ obsolete documentation files causing confusion
- Clean up 80+ scattered debug/test files
- Organize repository to professional standards
- Remove documentation fraud and outdated information

### Priority 2: SIMPLIFY BUILD SYSTEM (Issue #724)
**DEVELOPER EXPERIENCE**: Simple, working build process
**ESSENTIAL FIX**:
- One working build command only
- One working test command only
- Remove broken coverage setup
- Eliminate multiple broken build scripts

### Priority 3: REALISTIC ARCHITECTURE (Issue #725)
**TEAM EFFECTIVENESS**: Architecture team can actually implement
**ESSENTIAL FIX**:
- Reduce DESIGN.md to working essentials only
- Remove unimplementable architecture dreams
- Focus on basic functionality that works
- Set achievable goals matching team capability

### Priority 4: CORE FUNCTIONALITY VALIDATION
**WORKING SOFTWARE**: Verify basics work with simplified approach
**ESSENTIAL FIX**:
- Test basic parsing works without complex requirements
- Verify codegen produces working output for simple cases
- Document what actually works vs architectural claims
- Establish realistic baseline for development

### ESSENTIAL VALIDATION TESTS

**TEST 1: Basic Build**
```bash
fmp build --flag "-cpp -fmax-stack-var-size=131072"
```
**SHOULD WORK**: System compiles without errors

**TEST 2: Simple Parsing**
```fortran
program hello
  print *, "Hello, World!"
end program
```
**SHOULD WORK**: Parses and generates working Fortran

**TEST 3: Core Test Suite**
```bash
fmp test test_lexer_basic --flag "-cpp -fmax-stack-var-size=131072"
```
**SHOULD WORK**: Basic tests execute successfully

**ESSENTIAL SUCCESS CRITERIA**: Basic functionality works with simplified approach

## ✅ ESSENTIALS-FIRST DEVELOPMENT APPROACH

### Simplified Development Strategy
**FOCUS ON WORKING BASICS**: Build foundation that team can manage
- **Phase 1**: Clean repository - remove confusion and complexity
- **Phase 2**: Simplify build system - one command builds, one command tests
- **Phase 3**: Essential functionality - basic parsing and codegen working
- **Phase 4**: Realistic architecture - design team can actually implement

**New Process**: Achievable goals with simplified guidance

**FPM-FIRST ARCHITECTURE**
- **Dependency Management**: FPM automatically handles all tool dependencies via fpm.toml
- **Pure Fortran Interface**: External tools integrate through standard Fortran modules
- **Simple Integration**: Tools add `fortfront` as dependency - FPM handles the rest
- **No Build Complexity**: FPM manages module compilation and linking automatically

## What We Have Now

**WORKING**: Basic lexer, parser, and codegen that handles simple Fortran programs  
**TESTED**: Core functionality with test suite  
**BUILDABLE**: System compiles and runs with FPM

## What We Need

**CLEAN**: Remove repository bloat and complexity  
**SIMPLE**: Streamlined build process  
**REALISTIC**: Architecture that team can implement  
**FUNCTIONAL**: Focus on working basics first

---

# Essential Architecture (KEEP IT SIMPLE)

## Core Mission

fortfront is a **simple Fortran frontend library** that provides basic parsing and code generation functionality.

## Build Requirements (SIMPLE)

```bash
# Build fortfront
fmp build

# Run tests  
fmp test

# Use in other projects
# Add fortfront as dependency in fmp.toml
```

## Simple Architecture

```
Source Code → Lexer → Parser → AST → Codegen → Fortran
```

**FOCUS**: Basic parsing and code generation that works. No complex architecture until basics are solid.

## Essential Components

### What Actually Works
1. **Lexer**: Tokenizes Fortran source code
2. **Parser**: Builds basic AST from tokens  
3. **Codegen**: Generates Fortran output
4. **Tests**: Basic test suite for validation

### Current Status

**PRIORITY**: Clean repository and simplify build system first  
**NEXT**: Get basic functionality working reliably  
**FUTURE**: Add complexity only when essentials are solid  

---

## Essentials-First Development

**PRINCIPLE**: Build working software with simple, achievable goals  
**APPROACH**: Clean up complexity, focus on basics that work  
**OUTCOME**: Functional system that team can maintain and extend

*This simplified design replaces the previous 1700-line complex architecture that was beyond team capability.*