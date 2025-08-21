# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Build
```bash
# Standard build
fpm build --flag "-cpp -fmax-stack-var-size=65536"

# Or use the convenience script (recommended)
./build.sh
```

### Run all tests
```bash
# Standard test
fpm test --flag "-cpp -fmax-stack-var-size=65536"

# Or use the convenience script (recommended)
./test.sh
```

### Run tests with coverage (Linux only)
```bash
fpm clean --all
fpm test --profile debug --flag '-cpp -fmax-stack-var-size=65536 -fprofile-arcs -ftest-coverage -g'
lcov --capture --directory build/ --output-file coverage.info \
  --rc branch_coverage=1 \
  --ignore-errors inconsistent
lcov --remove coverage.info \
  'build/dependencies/*' \
  'test/*' \
  '/usr/*' \
  --output-file coverage_filtered.info
genhtml coverage_filtered.info --output-directory coverage_html \
  --branch-coverage \
  --legend
```

### Run a specific test
```bash
fpm test <test_name>
# Example: fpm test test_frontend_lexer_api
```

### Important build flags
- Always use `-cpp` flag when building/testing (required for preprocessing)
- Use `--profile debug` for debugging and coverage  
- **GCC 15.2.1 Compatibility**: Required flags for safe operation:
  - `-fmax-stack-var-size=65536` - Handles large module files
  - `-finit-derived` - Initialize derived type components safely

## Development Tips

- Do not always use `fpm clean all`. Only use clean in cases where all other fixes fail
- **GCC 15.2.1 Compatibility**: The compiler has allocatable component initialization bugs
  - Segmentation faults occur with default compiler settings on `poly_type_t` and other derived types with allocatable components
  - The issue manifests as crashes during type assignment operations and unallocated component access
  - The `-finit-derived` flag ensures all derived type components are properly initialized 
  - Use provided build scripts (./build.sh, ./test.sh) which include the necessary compatibility flags automatically
  - **CRITICAL**: Never build or test without the `-finit-derived` flag on GCC 15.2.1+
- **Avoid Using Shell Redirection Tricks**
  - Never use `2>&1` or similar shell redirection techniques blindly
  - These can mask underlying issues and make debugging more difficult

## Architecture Overview

fortfront is a Fortran frontend that processes code through four distinct phases, with input validation as a supporting module:

**Input Validation** (`src/input_validation.f90`) - Dedicated validation module (Issue #262)
   - Comprehensive input validation with enhanced error reporting
   - Standalone module independent of frontend transformation pipeline
   - Supports editor integration, build tools, and educational applications
   - Clean API with no circular dependencies

1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source code
   - Core module: `lexer_core.f90`
   - Produces `token_t` array

2. **Parser** (`src/parser/`) - Builds Abstract Syntax Tree (AST)
   - Core modules: `parser_core.f90`, `parser_dispatcher.f90`
   - Specialized parsers: `parser_expressions.f90`, `parser_declarations.f90`, `parser_control_flow.f90`
   - Uses `parser_state_t` to track parsing state
   - Produces AST nodes in a global arena

3. **Semantic Analysis** (`src/semantic/`) - Type checking and inference
   - Core module: `semantic_analyzer.f90`
   - Type system: `type_system_hm.f90` (Hindley-Milner type inference)
   - Scope management: `scope_manager.f90`
   - **Extensible Pipeline**: `semantic_pipeline.f90` (Issue #202)
   - Built-in analyzers: `builtin_analyzers.f90`
   - Analysis plugins: `usage_tracker_analyzer.f90`, `source_reconstruction_analyzer.f90`
   - Produces typed AST with `semantic_context_t`

4. **Code Generation** (`src/codegen/`) - Emits standard Fortran
   - Core module: `codegen_core.f90`
   - Handles indentation: `codegen_indent.f90`
   - Generates clean, standard-compliant Fortran code

### Key Design Patterns

- **AST Arena**: All AST nodes are stored in a global arena (`ast_core.f90`)
- **Visitor Pattern**: AST transformations use visitors (`ast_visitor.f90`)
- **Typed AST**: Extended AST nodes with type information (`ast_typed.f90`)
- **JSON Serialization**: Debug/inspection via JSON (`ast_json.f90`, `json_writer.f90`)

### Main Entry Points

- `frontend.f90`: Main module with `compile_source` subroutine
- `frontend_integration.f90`: Higher-level compilation interface

### Test Organization

Tests are organized by compilation phase:
- `test/lexer/` - Lexical analysis tests
- `test/parser/` - Parser tests
- `test/semantic/` - Semantic analysis and type inference tests
- `test/codegen/` - Code generation tests
- `test/standardizer/` - AST standardization tests
- `test/validation/` - Input validation module tests
- `test/error_reporting/` - Error reporting and validation tests

Total: 290 test files as of latest count.

### Error Reporting (Issue #256)

Enhanced error reporting provides comprehensive feedback:

- **Location Information**: Precise line and column numbers for all errors  
- **Clear Messages**: Specific problem identification with actionable descriptions
- **Fix Suggestions**: Helpful recommendations when syntax errors can be resolved
- **Source Context**: Display of problematic source lines with position indicators
- **No Silent Failures**: All syntax errors explicitly reported, no silent fallbacks
- **Validation Coverage**: Comments-only input and simple expressions properly accepted

### Common Development Patterns

When adding new features:
1. Start with lexer if new tokens are needed
2. Update parser to handle new syntax
3. Add semantic analysis for type checking
4. Implement code generation
5. Write comprehensive tests for each phase

### Semantic Analysis Extension Patterns

When adding new analysis capabilities:
1. **Core Analyzers**: Extend `semantic_analyzer_t` for essential standardization features
2. **Analysis Plugins**: Create specialized analyzers for external tool integration (fluff rules)
3. **Result Types**: Define clear result types with proper assignment operators
4. **Pipeline Integration**: Register analyzer in `semantic_pipeline.f90` type selection
5. **Testing**: Add comprehensive tests covering analysis methods and error cases
- we have four use cases for fortfront. 1. its own standardizer for lazy fortran. 2. fortrun that does module discovery and source and object cache. 3. fluff for static analysis and custom formatting of code, where it would be also ok to be very strict on dismissing original formatting (like ruff and black). 4. ffc the fortran fortran compiler that plugs fortfront to a hlfir llvm backend lowering chain like flang
- **Extensible Semantic Analysis (Issue #202)**: Plugin-based semantic analysis pipeline allows external tools (especially fluff) to perform custom code analysis using built-in analyzers and analysis plugins
- THE ASSIGNMENT OPERATOR ALWAYS HAS TO BE OVERLOADED TO DO A DEEP COPY FOR DERIVED TYPES WITH ALLOCATABLE MEMBERS

## Error Handling Architecture

fortfront uses structured error handling instead of `error_stop` for better library integration and user experience:

### Key Principles
- **Never use `error_stop`** in production code - use `result_t` pattern instead
- **Return structured errors** with context, suggestions, and severity levels
- **Enable graceful degradation** - components continue operating after errors
- **Support library integration** - host applications can handle errors appropriately

### Error Handling Patterns
```fortran
use error_handling

! Basic validation pattern
function validate_input(input) result(validation_result)
    type(result_t) :: validation_result
    
    if (invalid_condition) then
        validation_result = create_error_result( &
            "Specific error description", &
            ERROR_VALIDATION, &
            component="module_name", &
            context="function_name", &
            suggestion="How to fix this issue" &
        )
        return
    end if
    
    validation_result = success_result()
end function

! Factory result pattern (for node creation)
function safe_create_node(...) result(factory_result)
    type(factory_result_t) :: factory_result
    
    validation = validate_inputs(...)
    if (validation%is_failure()) then
        factory_result%result = validation
        return
    end if
    
    ! Create node successfully
    factory_result%node_index = new_index
    factory_result%result = success_result()
end function
```

### Error Severity Levels
- `ERROR_INFO`: Informational messages
- `ERROR_WARNING`: Issues that don't prevent operation
- `ERROR_ERROR`: Failures that prevent operation  
- `ERROR_CRITICAL`: Severe failures requiring immediate attention

### Error Categories
- `ERROR_VALIDATION`: Input validation failures
- `ERROR_TYPE_SYSTEM`: Type checking and inference errors
- `ERROR_MEMORY`: Memory allocation issues
- `ERROR_IO`: Input/output operation failures
- `ERROR_PARSER`: Parsing and syntax errors
- `ERROR_SEMANTIC`: Semantic analysis errors
- `ERROR_INTERNAL`: Internal consistency errors

See `docs/ERROR_HANDLING_GUIDE.md` for comprehensive examples and migration patterns.

## Memory Management

### Known Limitations

**IMPORTANT**: The current implementation has architectural limitations that need addressing:

### AST Node Assignment Operations
- AST node assignment operators currently **skip** copying `inferred_type` fields
- This breaks semantic information flow through the compilation pipeline
- **This is a temporary workaround**, not a design decision
- TODO: Implement cycle-safe deep copy for `mono_type_t` self-referential structures

### Type System Safety
- `mono_type_t` assignment uses limited shallow copying to prevent infinite recursion
- Function types (TFUN) require defensive handling due to incomplete type information
- Type variable names are always allocated to prevent finalizer crashes
- Nested `args` arrays are not copied to prevent performance issues and cycles

### Future Work Needed
1. Implement proper cycle detection in `mono_type_t` copying
2. Restore full `inferred_type` copying in AST nodes  
3. Remove defensive workarounds in semantic analyzer
4. Ensure type information preservation throughout pipeline