![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Features

- **Pure CLI Interface**: Transforms lazy Fortran to standard Fortran via stdin/stdout
- **High Performance**: <0.05ms average transformation time  
- **Type Inference**: Automatic variable declarations with type validation
- **Standard Compliant**: Clean, standard Fortran code generation
- **Integration**: Designed for [fortrun](https://github.com/lazy-fortran/fortrun) build orchestrator

## Building

```bash
fpm build && fpm test
```

## Usage

```bash
# Basic usage
echo "x = 42" | fortfront

# Function with automatic declarations
echo "function twice(x) result(y)
y = 2*x
end function" | fortfront
```

**Output:**
```fortran
program main
    implicit none
contains
    function twice(x) result(y)
        implicit none
        real(8), intent(in) :: x
        real(8) :: y
        y = 2*x
    end function twice
end program main
```

### Integration with fortrun

```bash
fortrun hello.lf  # Uses fortfront automatically
```

## Documentation

See `docs/` folder for detailed guides, API reference, and build instructions.

## License

MIT License - see LICENSE file for details.