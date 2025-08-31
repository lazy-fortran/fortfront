# CLAUDE.md - fortfront Essentials - BUILD SYSTEM STATUS UPDATE

**BUILD SYSTEM STATUS**: CMAKE has module compilation issues, FMP is primary working system
**CURRENT STATE**: FMP build system working, CMAKE needs module path fixes
**RECENT PROGRESS**: Major architecture refactoring completed, duplicate cleanup successful
**DEVELOPMENT ACTIVE**: Post-Sprint-6 system stabilization with accurate metrics

## WORKING BUILD COMMANDS

**FMP BUILD SYSTEM** (Primary - Working):
```bash
# FMP builds and executes tests successfully:
./build.sh  # Clean build completes
./test.sh   # Test suite runs (many failures but executes, times out after 30s)
```

**CMAKE BUILD SYSTEM** (Secondary - Module Issues):
```bash
# CMAKE has Fortran module path compilation errors:
make  # Error: Cannot copy Fortran module "arena_memory.mod"
```

## ESSENTIAL INFORMATION ONLY

### Required Flags
```bash
fpm build --flag "-cpp -fmax-stack-var-size=524288"
fpm test --flag "-cpp -fmax-stack-var-size=524288"
```

### Core Architecture 
1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source
2. **Parser** (`src/parser/`) - Builds AST 
3. **Semantic** (`src/semantic/`) - Type checking
4. **Codegen** (`src/codegen/`) - Emits Fortran

### Key Constraints
- Files: <1000 lines (1 edge case: semantic_analyzer.f90 at 1001 lines)
- Functions: <100 lines (0 violations: infer_type refactored to 72 lines)
- No `error_stop` in production (0 actual violations: AST infrastructure only)

### Current Problems
- CMAKE module compilation errors - needs path fixes
- Test suite has many logical failures but executes (timeout after 30s)
- ast_factory.f90 successfully split into modular components (Issue #714 RESOLVED)

### Essential Patterns
```fortran
use error_handling

! Return result_t instead of error_stop
function safe_operation() result(operation_result)
    type(result_t) :: operation_result
    
    if (error_condition) then
        operation_result = create_error_result("Error message")
        return
    end if
    
    operation_result = success_result()
end function
```

**GET BASIC FUNCTIONALITY WORKING FIRST**