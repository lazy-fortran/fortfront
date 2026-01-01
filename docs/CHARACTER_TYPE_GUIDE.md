# Character Type Handling

## Automatic Length Inference

fortfront calculates character lengths from string literals:

```bash
fortfront examples/lf/docs_character_length_inference.lf
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
fortfront examples/lf/docs_string_concatenation.lf
```
Output:
```fortran
program main
    implicit none
    character(len=11) :: message
    message = "hello" //" world"
end program main
```

## Reassignment With Different Lengths

When assigned strings of different lengths, uses a fixed-length character sized
to the longest value:

```bash
fortfront examples/lf/docs_variable_length_strings.lf
```
Output:
```fortran
program main
    implicit none
    character(len=5) :: message
    message = "hello"
    message = "hi"
end program main
```

Same-length assignments use fixed-length:
```bash
fortfront examples/lf/docs_fixed_length_reassignment.lf
```
Output: `character(len=3) :: code`

## Character Arrays

Arrays use maximum element length with padding:

```bash
fortfront examples/lf/docs_character_arrays.lf
```
Output:
```fortran
program main
    implicit none
    character(len=7) :: names(3)
    names = ["alice  ", "bob    ", "charlie"]
end program main
```

## Limitation

Function parameters used in character operations default to `real(8)` instead of being inferred from context. This is a known limitation.
