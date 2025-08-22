# Static Library Usage Guide

This document provides examples of how to use `libfortfront.a` static library from external C and Fortran programs.

## Library Location

After building with `fpm build`, the static library is located at:
```
build/gfortran_<hash>/fortfront/libfortfront.a
```

For convenience, it's also copied to the project root as `libfortfront.a`.

## C Interface Usage

### Example C Program

```c
#include <stdio.h>
#include <string.h>
#include "include/fortfront_c.h"

int main() {
    // Initialize the library
    if (fortfront_initialize() != 0) {
        printf("Failed to initialize fortfront\n");
        return 1;
    }
    
    // Get library information
    printf("Version: %s\n", fortfront_get_version());
    printf("Build: %s\n", fortfront_get_build_info());
    
    // Parse some Fortran code
    const char* source = "print *, 'Hello, World!'";
    int result = fortfront_parse_source(source, strlen(source));
    
    if (result != 0) {
        printf("Parse error: %s\n", fortfront_get_last_error());
    } else {
        printf("Parsing successful!\n");
    }
    
    // Clean up
    fortfront_cleanup();
    return 0;
}
```

### Compilation

```bash
gcc -I. -c your_program.c -o your_program.o
gfortran -o your_program your_program.o ./libfortfront.a -lgfortran
```

## Fortran Interface Usage

### Example Fortran Program

```fortran
program test_fortfront_external
    use fortfront_external_interface
    implicit none
    
    type(fortfront_result_t) :: result
    character(len=*), parameter :: source = "print *, 'Hello from Fortran!'"
    
    ! Transform source code
    result = fortfront_transform_source(source)
    
    if (result%success) then
        print *, "Transformation successful!"
        print *, "Output:"
        print *, result%output
    else
        print *, "Error:", result%error_message
    end if
end program test_fortfront_external
```

### Compilation

```bash
gfortran -I./build/gfortran_<hash> -o test_program test_program.f90 ./libfortfront.a -lgfortran
```

## Available Functions

### C Interface (`fortfront_c.h`)

- `int fortfront_initialize(void)` - Initialize library
- `void fortfront_cleanup(void)` - Clean up resources  
- `int fortfront_parse_source(const char* source_code, int length)` - Parse Fortran source
- `const char* fortfront_get_last_error(void)` - Get error message
- `void fortfront_clear_error(void)` - Clear error state
- `const char* fortfront_get_version(void)` - Get version string
- `const char* fortfront_get_build_info(void)` - Get build information

### Fortran External Interface

- `fortfront_transform_source(input_source)` - Transform Fortran source
- `fortfront_transform_source_with_format(input_source, format_opts)` - With formatting options
- `fortfront_compile_file(input_file, options)` - Compile from file

## Notes

- The static library includes all dependencies (json-fortran, stdlib)
- Link with `-lgfortran` when using with C programs
- Include appropriate module directories with `-I` flag for Fortran programs
- Library is thread-safe for basic operations
- Error handling uses structured result types, not exceptions

## Verification

To verify all symbols are exported:
```bash
nm libfortfront.a | grep "^[0-9a-f]* T fortfront_"
```

Should show all 7 C interface functions.