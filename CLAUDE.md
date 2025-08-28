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
- Files: <1000 lines (8 violations: largest is parser_import_statements.f90 at 1302 lines)
- Functions: <100 lines (violations still being assessed)
- No `error_stop` in production (81 violations: 69 in src/, 12 in test/)

### Current Problems
- CMAKE module compilation errors - needs path fixes
- Large files: 8 files over 1000 lines, largest is 1302 lines
- Test suite has many logical failures but executes (timeout after 30s)
- 81 error_stop violations (down from previous 1,386+)
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