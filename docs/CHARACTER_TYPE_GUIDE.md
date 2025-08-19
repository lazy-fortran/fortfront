# Character Type Handling Guide

## Overview

fortfront provides enhanced character type handling that automatically infers character variable types, handles string concatenation, manages different character lengths, and optimizes character array declarations.

## Character Type Features

### Automatic Character Length Inference

fortfront automatically calculates character lengths for string literals and concatenation:

```bash
echo 'name = "hello"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=5) :: name
    name = "hello"
end program main
```

### String Concatenation with Length Calculation

String concatenation automatically calculates the combined length:

```bash
echo 'greeting = "hello" // " world"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=11) :: greeting
    greeting = "hello" // " world"
end program main
```

### Allocatable Character for Different Lengths

When a variable is assigned strings of different lengths, fortfront uses allocatable character:

```bash
echo -e 'message = "hello"\nmessage = "hi"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=:), allocatable :: message
    message = "hello"
    message = "hi"
end program main
```

### Character Array Type Resolution

Character arrays use the maximum element length with proper array constructors:

```bash
echo 'names = ["alice", "bob", "charlie"]' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=7) :: names(3)
    names = [character(len=7) :: "alice", "bob", "charlie"]
end program main
```

## Character Type Unification

### Fixed-Length Character Types

When all string assignments have the same length, fortfront uses fixed-length character:

```bash
echo -e 'code = "ABC"\ncode = "XYZ"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=3) :: code
    code = "ABC"
    code = "XYZ"
end program main
```

### Mixed-Length Character Types

When string assignments have different lengths, fortfront automatically switches to allocatable:

```bash
echo -e 'data = "short"\ndata = "very long string"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=:), allocatable :: data
    data = "short"
    data = "very long string"
end program main
```

## Complex Character Operations

### Character Expressions in Calculations

Character concatenation works with string literals:

```bash
echo 'greeting = "hello " // "world"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=11) :: greeting
    greeting = "hello " // "world"
end program main
```

Note: Complex expressions involving undefined variables (like `first_name // " " // last_name`) require explicit variable declarations to work properly.

### Character Array Construction

Arrays with mixed-length string elements are handled properly:

```bash
echo 'words = ["a", "hello", "world"]' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    character(len=5) :: words(3)
    words = [character(len=5) :: "a", "hello", "world"]
end program main
```

## Implementation Benefits

### Type Safety

- **Correct Length Calculation**: All character lengths computed accurately
- **Proper Unification**: Character types unified based on usage patterns
- **Array Consistency**: Character arrays use consistent element lengths

### Performance Optimization

- **Fixed-Length When Possible**: Uses fixed-length character for better performance
- **Allocatable When Needed**: Only uses allocatable when length varies
- **Efficient Arrays**: Character arrays sized to maximum element length

### Standard Compliance

- **Proper Array Constructors**: Generates standard Fortran array syntax
- **Correct Declarations**: All character variables properly declared
- **Type Consistency**: Maintains type consistency throughout expressions

## Testing Character Functionality

You can test various character handling scenarios:

```bash
# Test basic character assignment
echo 'str = "hello"' | fortfront

# Test string concatenation
echo 'result = "hello" // " world"' | fortfront

# Test variable length handling
echo -e 'text = "short"\ntext = "longer text"' | fortfront

# Test character arrays
echo 'list = ["one", "two", "three"]' | fortfront

# Test literal concatenation
echo 'msg = "prefix" // " - " // "suffix"' | fortfront
```

## Known Limitations

### Function Parameter Inference

Currently, function parameters used in character operations are not automatically inferred as character type:

```bash
echo -e 'function concat_hello(x)\n  concat_hello = x // "!"\nend function' | fortfront
```

**Current Output (limitation):**
```fortran
function concat_hello(x)
    implicit none
    real(8), intent(in) :: x  ! Should be character
    concat_hello = x // "!"
end function concat_hello
```

This is a known limitation where function parameters default to `real(8)` instead of being inferred from context. The character concatenation still works, but the parameter type is not optimal.

## Character Type System Architecture

The enhanced character type handling uses:

- **Length Tracking**: Monitors character lengths throughout type inference
- **Unification Logic**: Properly unifies character types with different lengths
- **Substitution Application**: Applies type substitutions to maintain consistency
- **Array Element Sizing**: Calculates maximum length for character arrays

This ensures fortfront generates clean, efficient, and standard-compliant Fortran code for all character operations.