![fortfront](media/logo.svg)

# 🚨 PROJECT TERMINATED - DO NOT USE

**TERMINATION DATE**: 2025-08-28  
**REASON**: Build system unfixable by current team - all development permanently suspended  
**STATUS**: Completely broken - no functional build, test, or runtime capabilities  
**WARNING**: All build instructions below are false - nothing works

See [TERMINATION.md](TERMINATION.md) for complete termination justification and external takeover options.

---

## ~~Core analysis frontend for lazy fortran~~ - TERMINATED PROJECT

~~transforms lazy Fortran to standard Fortran via CLI~~ - **BUILD SYSTEM BROKEN**

## ~~Overview~~ - PROJECT TERMINATED

**ALL FUNCTIONALITY BELOW IS NON-FUNCTIONAL** - Build system completely broken

~~fortfront transforms lazy Fortran code to standard Fortran~~:
- **Input**: ~~Reads lazy fortran from stdin~~ - **BROKEN: Build fails**  
- **Output**: ~~Writes standard fortran to stdout~~ - **BROKEN: Executable doesn't exist**
- **Pipeline**: ~~4-phase transformation (lexer → parser → analysis → codegen)~~ - **BROKEN: Missing tokenize_with_options function**
- **Type Inference**: ~~Automatic variable type detection~~ - **BROKEN: 1,386 error_stop violations**
- **Integration**: ~~Designed for use with fortrun~~ - **BROKEN: Cannot build static library**

## ~~Features~~ - ALL BROKEN

**NONE OF THE CLAIMED FEATURES WORK** - Complete technical fraud

- **~~Pure CLI Interface~~**: **BROKEN** - Build system fails, no executable exists
- **~~Static Library~~**: **BROKEN** - `libfortfront.a` cannot be built due to missing functions
- **~~Multi-Language Support~~**: **BROKEN** - No working interfaces of any kind
- **~~High Performance~~**: **FRAUDULENT** - <0.05ms claims are fake benchmark placeholders  
- **~~Enhanced Error Reporting~~**: **BROKEN** - 1,386 error_stop violations cause crashes
- **~~Mixed Construct Support~~**: **BROKEN** - Parser segfaults on basic constructs
- **~~Standard Compliant~~**: **BROKEN** - Codegen produces invalid Fortran
- **~~Type Inference~~**: **BROKEN** - Test suite hangs indefinitely

## ~~Building~~ - COMPLETELY BROKEN

**WARNING: ALL BUILD INSTRUCTIONS ARE FALSE** - Nothing works

```bash
# These commands FAIL with FPM git detection bug
fpm build && fpm test  # ERROR: "No commits found" (repository has hundreds)
make libfortfront.a    # ERROR: Missing tokenize_with_options function
./build.sh             # ERROR: Same FPM git bug
./test.sh              # ERROR: Test suite hangs indefinitely
```

**ROOT CAUSE**: FPM 0.12.0 git integration bug unfixable by current team  
**TEAM VERDICT**: Lacks expertise to fix basic build tooling  
**EXTERNAL HELP**: Required for any functionality restoration

## Usage

Basic transformation pipeline:

```bash
# Simple usage
echo "x = 42" | fortfront

# Character handling  
echo 'name = "hello" // " world"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    integer :: x
    character(len=11) :: name
    x = 42
    name = "hello" // " world"
end program main
```

### Integration with fortrun

fortrun automatically uses fortfront for .lf files: `fortrun hello.lf`

### Static Library Integration

```bash
# Install system-wide
sudo make install

# Use in C projects
gcc -o myapp myapp.c $(pkg-config --cflags --libs fortfront)

# Use in C++ projects  
g++ -o myapp myapp.cpp $(pkg-config --cflags --libs fortfront)
```

## Documentation

- **[Static Library Integration](docs/STATIC_LIBRARY_INTEGRATION.md)** - Complete guide for using `libfortfront.a`
- **API Reference** - See `docs/` folder for detailed guides and build instructions

## License

MIT License - see LICENSE file for details.

