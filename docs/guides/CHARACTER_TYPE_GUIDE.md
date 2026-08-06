# Character Type Handling

## Automatic Length Inference

fortfront calculates character lengths from string literals:

```bash
fortfront examples/lf/docs_character_length_inference.lf
```
Output: `examples/f90/docs_character_length_inference_out.f90` (see `character(len=5) :: name`)

## String Concatenation

Combined lengths are calculated automatically:

```bash
fortfront examples/lf/docs_string_concatenation.lf
```
Output: `examples/f90/docs_string_concatenation_out.f90` (see `character(len=11) :: message`)

## Reassignment With Different Lengths

When assigned strings of different lengths, uses a fixed-length character sized
to the longest value:

```bash
fortfront examples/lf/docs_variable_length_strings.lf
```
Output: `examples/f90/docs_variable_length_strings_out.f90` (see `character(len=5) :: message`)

Same-length assignments use fixed-length:
```bash
fortfront examples/lf/docs_fixed_length_reassignment.lf
```
Output: `examples/f90/docs_fixed_length_reassignment_out.f90` (see `character(len=3) :: code`)

## Character Arrays

Arrays use maximum element length with padding:

```bash
fortfront examples/lf/docs_character_arrays.lf
```
Output: `examples/f90/docs_character_arrays_out.f90` (see `character(len=7) :: names(3)`)

## Substrings of Character-Array Elements

An element designator remains the substring base, so the parser preserves the
full `c(2)` identity for reads, writes, overlapping assignments, and actual
arguments such as `call consume(c(2)(1:3))`:

```bash
fortfront examples/f90/issue_669_nested_character_substring.f90
```

## Limitation

Function parameters used in character operations default to `real(dp)` instead of being inferred from context. This is a known limitation.
