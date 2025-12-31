# Character Type Handling

## Automatic Length Inference

fortfront calculates character lengths from string literals:

```bash
echo 'name = "hello"' | fortfront
```
Output:
```fortran
program main
    implicit none
    character(len=5) :: name
    name = "hello"
end program main
```

## String Concatenation

Combined lengths are calculated automatically:

```bash
echo 'greeting = "hello" // " world"' | fortfront
```
Output:
```fortran
program main
    implicit none
    character(len=11) :: greeting
    greeting = "hello" // " world"
end program main
```

## Variable-Length Strings

When assigned strings of different lengths, uses allocatable:

```bash
echo -e 'message = "hello"\nmessage = "hi"' | fortfront
```
Output:
```fortran
program main
    implicit none
    character(len=:), allocatable :: message
    message = "hello"
    message = "hi"
end program main
```

Same-length assignments use fixed-length:
```bash
echo -e 'code = "ABC"\ncode = "XYZ"' | fortfront
```
Output: `character(len=3) :: code`

## Character Arrays

Arrays use maximum element length with proper constructors:

```bash
echo 'names = ["alice", "bob", "charlie"]' | fortfront
```
Output:
```fortran
program main
    implicit none
    character(len=7) :: names(3)
    names = [character(len=7) :: "alice", "bob", "charlie"]
end program main
```

## Limitation

Function parameters used in character operations default to `real(8)` instead of being inferred from context. This is a known limitation.
