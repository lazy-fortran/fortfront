# CLAUDE.md - fortfront Essentials - CMAKE BUILD SYSTEM ACTIVE

**BUILD SYSTEM STATUS**: CMAKE implementation working - test executables building successfully  
**CURRENT STATE**: Hybrid CMAKE/FMP build system operational  
**RECENT PROGRESS**: CMakeLists.txt integrated, cmake_build directory populated  
**DEVELOPMENT ACTIVE**: Project progressing with working build infrastructure

## WORKING BUILD COMMANDS

**CMAKE BUILD SYSTEM** (Primary - Working):
```bash
# CMAKE builds successfully producing test executables:
mkdir -p cmake_build && cd cmake_build
cmake ..
make
```

**FMP BUILD SYSTEM** (Backup - Issue #712):
```bash
# FMP builds with known issues:
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
- FMP build system issues (Issue #712) - CMAKE working as alternative
- 35 functions over 100 lines (Issue #717)
- ast_factory.f90 is 1911 lines (Issue #714)
- 1,386 error_stop violations (Issue #716)
- Test coverage improvements needed

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