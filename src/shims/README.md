# Shims

## Purpose

This directory provides compatibility shims for external libraries that may not be available or may have incompatible interfaces. Shims provide a stable interface while allowing the underlying implementation to vary based on availability or platform.

## File Index

| File | Description |
|------|-------------|
| json_module.f90 | JSON library shim (compatibility layer for json-fortran) |

## Key Concepts

**Why Shims?**
- **Optional dependencies**: Gracefully handle missing libraries
- **API stability**: Isolate fortfront from external API changes
- **Platform compatibility**: Handle platform-specific variations
- **Testing**: Mock external dependencies for unit tests

**JSON Shim**
- Provides JSON parsing and serialization
- Wraps json-fortran library if available
- Falls back to minimal implementation if unavailable
- Used for configuration files and structured output

**Shim Pattern**
```fortran
module json_module
    ! Public interface (stable)
    public :: json_parse, json_serialize

contains
    subroutine json_parse(input, output)
#ifdef HAVE_JSON_FORTRAN
        ! Use json-fortran library
        call json_fortran_parse(input, output)
#else
        ! Minimal fallback implementation
        call minimal_json_parse(input, output)
#endif
    end subroutine
end module
```

**Design Principles**
- **Minimal interface**: Expose only what fortfront needs
- **Feature detection**: Use preprocessor to detect availability
- **Graceful degradation**: Provide fallback when possible
- **Clear errors**: Fail clearly when feature unavailable and required

**When to Add Shims**
- External library with unstable API
- Optional dependency for non-critical features
- Platform-specific library with alternatives
- Dependency with licensing concerns

## Dependencies

**External (Optional)**
- `json-fortran` - JSON library (if available)

**Standard Library**
- Fortran standard library for fallback implementations
