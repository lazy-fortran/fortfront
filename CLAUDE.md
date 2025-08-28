# CLAUDE.md - fortfront Essentials

**🚨 CRITICAL INFRASTRUCTURE FAILURE**: Complete build system breakdown. Development impossible.

## BUILD SYSTEM STATUS: COMPLETELY BROKEN

**VALIDATION RESULTS (Issue #726 - Aug 28, 2025)**:
- ❌ `./build.sh` - FAILS with fatal git repository errors
- ❌ `./test.sh` - FAILS with fatal git repository errors  
- ❌ `fpm build` - FAILS with fatal git repository errors
- ❌ `fmp test` - FAILS with fatal git repository errors
- ❌ No working executables found in build directory
- ❌ No development workflow possible

**ROOT CAUSE**: FMP 0.12.0 has critical bug in git integration:
```
fatal: your current branch 'main' does not have any commits yet
<ERROR> *cmd_build* Model error: Error while retrieving commit information
STOP 1
```

**REALITY CHECK**: All documentation claiming "working build commands" is FALSE.

## NON-FUNCTIONAL BUILD COMMANDS

```bash
# ALL OF THESE FAIL:
./build.sh
./test.sh
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

### CRITICAL PROBLEMS (VALIDATED - Issue #726)

**INFRASTRUCTURE FAILURES**:
- ❌ **Build system completely broken** - FMP 0.12.0 git integration bug
- ❌ **Test suite completely broken** - Same FMP git integration bug  
- ❌ **No working development workflow** - Cannot build, test, or run anything
- ❌ **No compiled binaries** - Build directory empty of executables

**CODE QUALITY PROBLEMS (Cannot be fixed without working build)**:
- 35 functions over 100 lines (Issue #717)
- ast_factory.f90 is 1911 lines (Issue #714)  
- 1,386 error_stop violations (Issue #716)

**VALIDATION CONCLUSION**: System is non-functional for development

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

## VALIDATION SUMMARY (Issue #726)

**TESTED FUNCTIONALITY**: NONE - Cannot build or test any code

**WORKING FEATURES**: NONE - Build system completely broken

**REALISTIC CAPABILITIES**: NONE - Development workflow impossible  

**PRIORITY**: Fix FMP build system bug before any development work

**HONEST ASSESSMENT**: Repository contains source code but no working functionality due to infrastructure failure