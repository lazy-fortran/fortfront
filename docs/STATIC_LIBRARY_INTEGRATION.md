# Static Library Integration Guide

Complete guide for integrating `libfortfront.a` into external projects.

## Quick Start

```bash
# Build static library
make libfortfront.a

# Install system-wide
sudo make install

# Use in your project
gcc -o myapp myapp.c -lfortfront
```

## Building libfortfront.a

```bash
# Build the static library (creates libfortfront.a in project root)
make libfortfront.a

# Verify library was created successfully
ls -la libfortfront.a
file libfortfront.a
```

Expected output:
```
-rw-r--r-- 1 user user 4979712 Aug 22 23:54 libfortfront.a
libfortfront.a: current ar archive
```

## Installation

### System-wide Installation

```bash
# Install library, headers, and pkg-config file
sudo make install

# Custom installation prefix
sudo make install PREFIX=/opt/fortfront
```

Installation creates:
- `/usr/local/lib/libfortfront.a` - Static library
- `/usr/local/include/fortfront/*.mod` - Fortran module files  
- `/usr/local/include/fortfront_c.h` - C header
- `/usr/local/include/fortfront_cpp.hpp` - C++ header
- `/usr/local/lib/pkgconfig/fortfront.pc` - pkg-config file

### Local Project Installation

```bash
# Copy library and headers to your project
cp libfortfront.a /path/to/your/project/lib/
cp include/*.h /path/to/your/project/include/
```

## Language Integration Examples

### C Integration

**Header**: `fortfront_c.h`

```c
#include "fortfront_c.h"
#include <stdio.h>

int main() {
    // Initialize the library
    if (fortfront_initialize() != 0) {
        fprintf(stderr, "Failed to initialize fortfront\n");
        return 1;
    }
    
    // Parse some Fortran code
    const char* source = "x = 42\nprint *, x";
    if (fortfront_parse_source(source, strlen(source)) != 0) {
        fprintf(stderr, "Parse error: %s\n", fortfront_get_last_error());
        fortfront_cleanup();
        return 1;
    }
    
    printf("Fortfront version: %s\n", fortfront_get_version());
    
    // Cleanup
    fortfront_cleanup();
    return 0;
}
```

**Compile:**
```bash
# With pkg-config
gcc -o example example.c $(pkg-config --cflags --libs fortfront)

# Without pkg-config
gcc -o example example.c -L/usr/local/lib -lfortfront
```

### C++ Integration

**Header**: `fortfront_cpp.hpp`

```cpp
#include "fortfront_cpp.hpp"
#include <iostream>

int main() {
    try {
        // RAII library management
        auto lib = fortfront::create_library();
        
        // Parse Fortran source
        std::string source = "program hello\n  x = 42\n  print *, x\nend program";
        
        if (lib->parse_source(source)) {
            std::cout << "Parse successful!\n";
        } else {
            std::cout << "Parse failed\n";
        }
        
        std::cout << "Version: " << lib->get_version() << "\n";
        
    } catch (const fortfront::FortfrontError& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
```

**Compile:**
```bash
# With pkg-config
g++ -std=c++17 -o example example.cpp $(pkg-config --cflags --libs fortfront)

# Without pkg-config
g++ -std=c++17 -o example example.cpp -L/usr/local/lib -lfortfront
```

### Fortran Integration

**External Interface**: `fortfront_external_interface.f90`

```fortran
program external_example
    use fortfront_external_interface
    use error_handling
    implicit none
    
    character(len=*), parameter :: source = 'x = 42' // new_line('A') // 'print *, x'
    type(result_t) :: result
    character(len=:), allocatable :: version
    
    ! Compile source code
    call fortfront_external_compile(source, result)
    
    if (result%is_success()) then
        print *, 'Compilation successful!'
    else
        print *, 'Compilation failed: ', trim(result%message)
    endif
    
    ! Get version information
    version = fortfront_external_get_version()
    print *, 'Fortfront version: ', version
    
end program external_example
```

**Compile:**
```bash
# Link with fortfront module files
gfortran -o example example.f90 -I/usr/local/include/fortfront -L/usr/local/lib -lfortfront

# Or with pkg-config
gfortran -o example example.f90 $(pkg-config --cflags --libs fortfront)
```

### Rust Integration

**Cargo.toml:**
```toml
[dependencies]
fortfront-sys = { path = "path/to/fortfront/bindings/rust" }
```

**Rust code:**
```rust
use fortfront_sys::{Fortfront, FortfrontResult};

fn main() -> FortfrontResult<()> {
    // Create fortfront instance
    let fortfront = Fortfront::new()?;
    
    // Parse Fortran source
    let source = "program hello\n  x = 42\n  print *, x\nend program";
    fortfront.parse_source(source)?;
    
    println!("Parse successful!");
    println!("Version: {}", fortfront.get_version());
    
    Ok(())
}
```

**Build:**
```bash
# Ensure libfortfront.a is in library path or use PKG_CONFIG_PATH
cargo build
```

## Build System Integration

### CMake Integration

```cmake
# FindFortfront.cmake
find_package(PkgConfig REQUIRED)
pkg_check_modules(FORTFRONT REQUIRED fortfront)

# Link to your target
target_link_libraries(your_target ${FORTFRONT_LIBRARIES})
target_include_directories(your_target PRIVATE ${FORTFRONT_INCLUDE_DIRS})
```

### Meson Integration

```meson
# meson.build
fortfront_dep = dependency('fortfront', method: 'pkg-config')

executable('your_app', 'main.c', 
           dependencies: fortfront_dep)
```

### Autotools Integration

```makefile
# configure.ac
PKG_CHECK_MODULES([FORTFRONT], [fortfront])

# Makefile.am
your_app_CFLAGS = $(FORTFRONT_CFLAGS)
your_app_LDADD = $(FORTFRONT_LIBS)
```

## Cross-Platform Compatibility

### Linux
Standard build and installation process works on all distributions.

### macOS
```bash
# Install dependencies if needed
brew install gfortran

# Build and install normally
make libfortfront.a
sudo make install
```

### Windows (MinGW/MSYS2)
```bash
# In MSYS2 environment
pacman -S mingw-w64-x86_64-gcc-fortran

# Build library
make libfortfront.a

# Install to system
make install PREFIX=/mingw64
```

## Verification and Testing

### Verify Installation

```bash
# Check library is installed
ls -la /usr/local/lib/libfortfront.a

# Check pkg-config works
pkg-config --modversion fortfront
pkg-config --cflags --libs fortfront

# Test with simple program
echo '#include "fortfront_c.h"
int main() { return fortfront_initialize(); }' > test.c
gcc test.c $(pkg-config --cflags --libs fortfront) -o test
./test && echo "Library works!"
```

### Dependency Analysis

```bash
# Run included dependency analysis script
./scripts/analyze_dependencies.sh

# Verify no unexpected external dependencies
nm libfortfront.a | grep ' U ' | head -10
```

### Symbol Verification

```bash
# Check library contains expected symbols  
./scripts/verify_symbols.sh

# Basic symbol count
nm libfortfront.a | grep ' T ' | wc -l
```

## Common Integration Issues

### Missing Fortran Runtime
**Error:** `undefined reference to _gfortran_*`
**Solution:** Link with gfortran runtime: `-lgfortran`

### Module File Location
**Error:** `Can't open module file`  
**Solution:** Add module path: `-I/usr/local/include/fortfront`

### Library Not Found
**Error:** `cannot find -lfortfront`
**Solution:** Add library path: `-L/usr/local/lib`

### pkg-config Not Found
**Error:** `Package 'fortfront' was not found`
**Solution:** Check PKG_CONFIG_PATH includes `/usr/local/lib/pkgconfig`

## API Reference

### C Interface (`fortfront_c.h`)

```c
// Library lifecycle
int fortfront_initialize(void);
void fortfront_cleanup(void);

// Basic parsing
int fortfront_parse_source(const char* source_code, int length);

// Error handling  
const char* fortfront_get_last_error(void);
void fortfront_clear_error(void);

// Library information
const char* fortfront_get_version(void);
const char* fortfront_get_build_info(void);
```

### C++ Interface (`fortfront_cpp.hpp`)

```cpp
namespace fortfront {
    class Library {
    public:
        Library();  // Initializes library
        ~Library(); // Cleanup
        
        bool parse_source(const std::string& source_code);
        std::string get_version() const;
        std::string get_build_info() const;
        bool is_initialized() const;
    };
    
    std::unique_ptr<Library> create_library();
}
```

### Fortran Interface (`fortfront_external_interface`)

```fortran
! Compilation interface
subroutine fortfront_external_compile(source_code, compilation_result)
    character(len=*), intent(in) :: source_code
    type(result_t), intent(out) :: compilation_result
end subroutine

! Information functions
function fortfront_external_get_version() result(version_string)
function fortfront_external_get_build_info() result(build_info)
```

## Advanced Usage

### Custom Build Flags

```bash
# Build with specific compiler flags
FPM_FFLAGS="-O3 -march=native" make libfortfront.a

# Debug build
FPM_FFLAGS="-g -fcheck=all" make libfortfront.a
```

### Static Linking Verification

```bash
# Ensure fully static linking
ldd your_program
# Should show "not a dynamic executable" for fully static builds

# Or check for minimal dependencies
ldd your_program | grep -v "linux-vdso\|ld-linux"
```

### Performance Optimization

```bash
# Build optimized library
FFLAGS="-O3 -flto" make libfortfront.a

# Profile-guided optimization
FFLAGS="-fprofile-generate" make libfortfront.a
# Run representative workload, then:
FFLAGS="-fprofile-use" make libfortfront.a
```

## Library Architecture

The static library contains:
- **Core Components**: Lexer, parser, semantic analyzer, code generator
- **Zero Dependencies**: No external runtime dependencies except standard library
- **Multi-Language Support**: C, C++, Fortran, and Rust interfaces
- **Self-Contained**: All fortfront functionality in single archive file

**Size:** ~5MB containing complete fortfront analysis pipeline
**Performance:** <0.05ms average transformation time
**Compatibility:** Works with GCC, Clang, Intel compilers on Linux, macOS, Windows