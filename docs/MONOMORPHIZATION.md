# Type Inference and Monomorphization Analysis

## Overview

This document analyzes fortfront's current type inference approach and compares it with monomorphization techniques used in other languages. It explores potential extensions to support multiple type specializations while maintaining fortfront's design philosophy of minimal syntax.

## Current Fortfront Behavior

### What It Does

Fortfront currently performs **single-file type inference** from call sites:

```fortran
! Input: lazy fortran (.lf)
function add(a, b)
    add = a + b
end function

x = add(5, 3)
y = add(2.5, 1.5)
```

**Current output:** Picks the first type signature encountered and uses it for all calls.

**Limitation:** If multiple type signatures are used, only one is generated, causing type errors for others.

### How It Works

1. **Parse** the entire file into AST
2. **Collect call sites** and their argument types (from literals)
3. **Infer types** for function parameters based on first/chosen call site
4. **Generate** standard Fortran with inferred types
5. **Emit** to single program or module

**Scope:** Single file only - cannot perform cross-file inference.

## Monomorphization: What Other Languages Do

### Definition

**Monomorphization** is generating separate specialized code for each concrete type combination used with a generic function.

### C++ Templates (Compile-Time)

```cpp
template<typename T>
T add(T a, T b) { return a + b; }

int main() {
    add(5, 3);      // Generates: add<int>
    add(2.5, 1.5);  // Generates: add<double>
}
```

**Process:**
- Programmer explicitly declares generic with `template<typename T>`
- Compiler instantiates template for each type used
- Results in multiple functions in binary
- Zero runtime overhead

**Trade-offs:**
- ✓ Zero-cost abstraction
- ✓ Type-specific optimizations
- ✗ Verbose syntax (requires `template<typename T>`)
- ✗ Code bloat (multiple copies)
- ✗ Slow compilation

### Rust Generics (Compile-Time)

```rust
fn add<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

fn main() {
    add(5, 3);      // Generates: add::<i32>
    add(2.5, 1.5);  // Generates: add::<f64>
}
```

**Process:**
- Explicit generic syntax: `<T: Trait>`
- Type checking against trait bounds before monomorphization
- LLVM generates specialized code per type
- Aggressive optimization and inlining

**Trade-offs:**
- ✓ Zero-cost abstraction
- ✓ Early error detection (trait bounds)
- ✓ Better error messages than C++
- ✗ Requires trait bound syntax
- ✗ Code bloat

### ML Family (Compile-Time Polymorphism)

```ocaml
let add a b = a + b
(* Inferred type: int -> int -> int *)
```

**Process:**
- **Hindley-Milner type inference** infers most general polymorphic type
- Compiles to **single function** that works for all types
- Uses **uniform representation** (pointer-sized values)
- **Type erasure** or **dictionary passing** at runtime

**Key Difference:** Does NOT monomorphize - one implementation for all types!

**Trade-offs:**
- ✓ No type annotations needed
- ✓ Fast compilation (compile once)
- ✓ Small binaries
- ✓ Cross-module polymorphism
- ✯ Small runtime overhead (type erasure/boxing)

### Julia (Runtime JIT Specialization)

```julia
function add(a, b)
    return a + b
end

add(5, 3)      # First call: JIT compile for Int64
add(2.5, 1.5)  # First call: JIT compile for Float64
```

**Process:**
- Parse once, infer types locally
- On **first call** with specific types: JIT compile specialized version
- Cache compiled code in memory
- Subsequent calls use cached version

**Trade-offs:**
- ✓ No type annotations
- ✓ Fast startup (lazy compilation)
- ✓ Works across packages
- ✓ Can handle runtime types
- ✗ "Time to first plot" - first call slow
- ✗ Memory overhead (multiple cached versions)

## Potential Fortfront Extension: Usage-Driven Monomorphization

### The Idea

Instead of picking one type signature, **generate all signatures used in the file**:

```fortran
! Input: lazy fortran
function add(a, b)
    add = a + b
end function

x = add(5, 3)       ! Use with integers
y = add(2.5, 1.5)   ! Use with reals
```

**Enhanced output:**
```fortran
module auto_add
    implicit none

    interface add
        module procedure add_int_int, add_real_real
    end interface add

contains

    function add_int_int(a, b) result(res)
        integer, intent(in) :: a, b
        integer :: res
        res = a + b
    end function add_int_int

    function add_real_real(a, b) result(res)
        real, intent(in) :: a, b
        real :: res
        res = a + b
    end function add_real_real

end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real :: y

    x = add(5, 3)        ! Resolves to add_int_int
    y = add(2.5, 1.5)    ! Resolves to add_real_real
end program main
```

### Algorithm

1. **Parse** entire file
2. **Collect all call sites** for each function
3. **Extract unique type signatures** from literal types
4. **For each unique signature:**
   - Clone function body AST
   - Substitute types throughout
   - Generate mangled name (e.g., `add_int_int`)
5. **Create generic interface** binding all variants
6. **Emit module** with interface + specialized functions

### Advantages

**✓ Minimal syntax:** No type annotations needed (like current fortfront)

**✓ Multiple specializations:** Works correctly with different types

**✓ Zero runtime cost:** Static dispatch via Fortran interfaces

**✓ Fortran-native:** Uses standard Fortran generic interfaces

**✓ Type-specific optimization:** Each version optimized for its types

### Limitations

**✗ Single-file only:** Cannot infer across file boundaries

**✗ Literal types only:** Cannot infer from variables:
```fortran
! This works:
x = add(5, 3)  ! Literal integers

! This doesn't:
a = get_value_from_file()
x = add(a, 3)  ! Type of 'a' unknown at compile time
```

**✗ Code bloat:** Multiple copies of function body

**✗ No type constraints:** Cannot express "T must support +" explicitly

**✗ Compilation overhead:** More code to generate and compile

### Comparison Matrix

| Aspect | Current Fortfront | Enhanced Fortfront | C++/Rust | ML | Julia |
|--------|-------------------|-------------------|----------|-----|-------|
| **Syntax** | Inferred | Inferred | Explicit | Inferred | Inferred |
| **Specializations** | 1 per function | N per signatures | N per types | 1 polymorphic | N (JIT) |
| **When** | Compile-time | Compile-time | Compile-time | Compile-time | Runtime |
| **Scope** | Single file | Single file | Cross-file | Cross-module | Cross-package |
| **Runtime cost** | Zero | Zero | Zero | Small | Zero (after JIT) |
| **Code size** | Small | Medium | Large | Small | Medium (JIT cache) |
| **Constraints** | None | None | Yes (concepts) | Yes (type classes) | Yes (traits) |

## Design Philosophy Considerations

### Fortfront's Current Niche

Fortfront targets:
- **Quick numerical scripts** - single-file programs
- **Prototyping** - rapid iteration
- **Teaching** - minimal cognitive overhead
- **Legacy modernization** - gradual type introduction

**Design Principle:** "Write simple code, compiler handles boilerplate"

### Trade-offs for Monomorphization

**Pros for fortfront:**
- Aligns with "inferred types" philosophy
- Leverages Fortran's native generic interfaces
- Numerical code often needs int/real/complex variants
- Single-file scope is tractable

**Cons for fortfront:**
- Increases complexity significantly
- May confuse users (which version am I calling?)
- Code bloat less acceptable in scripts
- Breaks single-file compilation model (needs module system)

### Alternative: Explicit Opt-In

Instead of automatic monomorphization, provide **explicit syntax** for generic intent:

```fortran
! Hypothetical syntax - explicit generic
generic function add(a, b)
    add = a + b
end function

! Or stick with current: pick one type, fail on others
! Let users write multiple versions manually if needed
```

## Recommendations

### For Current Fortfront

**Keep current behavior** for now:
1. Single type signature per function
2. Clear error messages when multiple types used
3. Suggest manual interface creation for generic needs

**Rationale:**
- Simplicity is core value
- Single-file scope limits utility of monomorphization
- Most numerical scripts use consistent types

### For Future Enhancement

**If pursuing monomorphization:**

1. **Start with limited scope:**
   - Only numeric types (integer, real, complex)
   - Maximum N specializations per function
   - Clear warnings about code generation

2. **Provide visibility:**
   - Show which specializations were generated
   - Option to output intermediate Fortran (already exists)
   - Clear naming for generated functions

3. **Documentation:**
   - Explain single-file limitation clearly
   - Show when to use vs manual interfaces
   - Performance implications

4. **Consider hybrid:**
   - Default: pick first type (current behavior)
   - Flag: `--monomorphize` enables multi-specialization
   - Users choose based on needs

## Related Work

### Languages with Similar Goals

**Nim:**
- Compiles to C
- Type inference
- Supports generics via templates
- Cross-file scope

**Crystal:**
- Ruby-like syntax
- Type inference
- Compile-time monomorphization
- LLVM backend

**D:**
- Template metaprogramming
- Optional type inference
- Compile-time function generation

### Key Insight from Julia

Julia proves that:
- Runtime monomorphization works well
- Type inference + specialization is powerful
- "Write simple code, get fast code" is achievable

But Julia has:
- JIT compiler (fortfront is AOT)
- Package system (fortfront is single-file)
- LLVM backend (fortfront emits Fortran)

**Lesson:** The concept is sound, but implementation context matters greatly.

## Conclusion

**Current fortfront:** Type inference from single call site, one specialization.

**Monomorphization:** Generate multiple specializations per type signature.

**Is it worth it?**
- **For single-file numerical scripts:** Marginal benefit
- **For library development:** Limited by single-file scope
- **For teaching:** Adds complexity
- **For performance:** Fortran already optimizes well

**Recommendation:**
- Document current behavior clearly
- Improve error messages for multi-type usage
- Consider monomorphization as opt-in future feature
- Focus on core mission: simple lazy Fortran for scripts

The ML family shows polymorphism works without monomorphization. Julia shows monomorphization works beautifully at runtime. C++/Rust show it works at compile-time with explicit syntax. Fortfront's automatic compile-time approach would be novel, but serves a narrow use case.

**The current design is sound for fortfront's scope.** Enhancements should preserve simplicity while adding capability where it truly matters.
