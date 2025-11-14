# Interfaces

## Purpose

This directory provides C language bindings for the fortfront library, enabling integration with non-Fortran tools and build systems. The C interface exposes fortfront's core functionality (parsing, semantic analysis, code generation) through a C-compatible API.

## File Index

| File | Description |
|------|-------------|
| fortfront_c_interface.f90 | C-compatible API bindings using ISO_C_BINDING |

## Key Concepts

**C Interoperability**
- Uses Fortran's `iso_c_binding` module for C compatibility
- Fortran procedures exported with `bind(C)` attribute
- C-compatible types: `c_int`, `c_char`, `c_ptr`, etc.
- String conversion between Fortran and C conventions

**API Functions**
- **Parse**: Parse Fortran source to AST
- **Analyze**: Run semantic analysis on AST
- **Generate**: Generate standard Fortran from AST
- **Transform**: Full pipeline (parse → analyze → generate)
- **Error handling**: Return error codes, error messages

**Memory Management**
- Fortran manages memory internally (arena allocation)
- C API provides opaque handles (pointers to Fortran objects)
- C code must call cleanup functions to free resources
- No manual deallocation of individual objects

**String Handling**
- C strings: Null-terminated character arrays
- Fortran strings: Length-specified character arrays
- Interface converts between conventions
- UTF-8 encoding assumed

**Use Cases**
- **Build systems**: Integrate fortfront into non-Fortran build tools
- **Language servers**: LSP implementations in C/C++/Rust
- **IDEs**: Editor integrations (VSCode, Vim, etc.)
- **Code analysis tools**: Static analyzers, formatters, linters
- **Bindings**: Generate bindings for Python, JavaScript, etc.

**Error Handling**
- Return error codes for failures
- Error messages returned as C strings
- Caller responsible for checking error codes
- Detailed errors available via error API

**Typical C Usage Pattern**
```c
#include "fortfront.h"

int main() {
    // Initialize fortfront
    fortfront_context_t* ctx = fortfront_create_context();

    // Parse source
    fortfront_ast_t* ast = NULL;
    int err = fortfront_parse(ctx, source_code, &ast);
    if (err != 0) {
        fprintf(stderr, "Parse error: %s\n", fortfront_get_error(ctx));
        return 1;
    }

    // Semantic analysis
    err = fortfront_analyze(ctx, ast);
    if (err != 0) {
        fprintf(stderr, "Analysis error: %s\n", fortfront_get_error(ctx));
        return 1;
    }

    // Generate code
    char* output = NULL;
    err = fortfront_generate(ctx, ast, &output);
    if (err != 0) {
        fprintf(stderr, "Generation error: %s\n", fortfront_get_error(ctx));
        return 1;
    }

    printf("%s\n", output);

    // Cleanup
    fortfront_free_string(output);
    fortfront_destroy_ast(ast);
    fortfront_destroy_context(ctx);

    return 0;
}
```

## Dependencies

**Fortran Standard Library**
- `iso_c_binding` - C interoperability

**Frontend**
- `frontend/` - Transformation pipeline

**Semantic Analysis**
- `semantic/` - Type inference and validation

**Codegen**
- `codegen/` - Standard Fortran generation

**Error Handling**
- `error_handling` - Error reporting
