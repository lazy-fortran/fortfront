# CLAUDE.md - fortfront Essentials

**CRITICAL SYSTEM STATUS**: Build system is broken (Issue #712). Basic functionality not working.

## WORKING BUILD COMMANDS

**WARNING**: These commands currently fail due to missing `tokenize_with_options` function in lexer_core.

```bash
# These SHOULD work but don't:
./build.sh
./test.sh
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
- Files: <1000 lines (many violations exist)
- Functions: <100 lines (many violations exist)
- No `error_stop` in production (1,386+ violations exist)

### Current Problems
- Build system broken (Issue #712)
- 35 functions over 100 lines (Issue #717)
- ast_factory.f90 is 1911 lines (Issue #714)
- 1,386 error_stop violations (Issue #716)
- Test suite broken

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