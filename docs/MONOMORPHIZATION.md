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

## Fortfront Implementation Details

### Current Architecture

**Type Inference Pipeline** (semantic_analyzer.f90):
1. **Context Setup** - `create_semantic_context` builds scope stack, installs intrinsic bindings
2. **Program Walk** - `analyze_program_node_arena` iterates AST using lightweight stack
3. **Statement Inference** - Specialized analyzers infer/refine types:
   - `semantic_assignment_inference` - Variable type inference from assignments
   - `semantic_function_analysis` - Function parameter and return type inference
   - `semantic_binary_operations` - Expression type inference
4. **Call Graph** - Optional `call_graph_module::build_call_graph` after semantics

**Code Generation** (codegen modules):
- `codegen_declarations_procedures.f90` - Function/subroutine signature generation
- `codegen_declarations_programs.f90` - Module and interface block generation
- `codegen_statements.f90` - Statement code emission
- Already supports `interface_block_node` and `module_procedure_node`

**Key Insight:** Fortfront already has all infrastructure needed for monomorphization:
- ✓ Call graph tracks all call sites with type information
- ✓ Interface block generation exists
- ✓ Module procedure support exists
- ✓ AST cloning and manipulation possible

### Cross-Module Specialization: Practical Path Forward

Based on analysis of Fortfront's current architecture and standard Fortran capabilities, here is the recommended implementation strategy:

## The Fortran-Native Approach: Caller-Side Augmentation

### Core Principle

**Use Fortran's generic interface extension mechanism** to allow caller-side specialization without mutating library objects.

### How It Works

**Library defines base generic:**
```fortran
module m_add
  implicit none
  private
  public :: add

  interface add
    module procedure add_int_int
  end interface add
contains
  integer function add_int_int(a, b)
    integer, intent(in) :: a, b
    add_int_int = a + b
  end function add_int_int
end module m_add
```

**Caller extends generic locally:**
```fortran
module m_caller_add_ext
  use m_add, only: add   ! Bring generic name into scope
  implicit none
  private
  public :: add          ! Re-export extended generic

  interface add          ! Extend with new specifics
    module procedure add_real_real
  end interface add
contains
  real function add_real_real(a, b)
    real, intent(in) :: a, b
    add_real_real = a + b
  end function add_real_real
end module m_caller_add_ext
```

**Use site:**
```fortran
program main
  use m_add              ! Base specifics
  use m_caller_add_ext   ! Augmented specifics
  implicit none
  integer :: xi
  real :: xr

  xi = add(2, 3)         ! Resolves to add_int_int (library)
  xr = add(2.5, 1.5)     ! Resolves to add_real_real (caller)
end program main
```

**Key mechanism:** Fortran merges visible specifics from both modules into the generic set at use site.

### Fortfront Implementation Strategy

**Pattern 1: Single-File Scripts (Current)**
- Continue current behavior: one specialization per function
- Emit as program with contained procedures

**Pattern 2: Cross-Module Usage (New)**
- **Library mode:** Emit module with base specialization(s)
- **Caller mode:** Generate instantiation module:
  1. Analyze call sites in caller code
  2. Determine needed type signatures not in library
  3. Generate `<library>_inst_<caller>.f90` with:
     - `use <library>, only: <generic_name>`
     - `interface <generic_name>` with new specifics
     - Implementation of new specifics
  4. Caller imports both library and instantiation module

**Pattern 3: Prebuilt Specializations (Optional)**
- Library ships multiple common specializations upfront
- Reduces need for caller-side generation
- Example: `add_int32_int32`, `add_real64_real64`, `add_complex64_complex64`

### Naming Convention

**Deterministic mangling to avoid collisions:**
- Format: `<name>__<kind1>_<kind2>_...`
- Examples:
  - `add__i32_i32` - integer(4) + integer(4)
  - `add__r64_r64` - real(8) + real(8)
  - `add__r64rank1_r64rank1` - real(8),dimension(:) + real(8),dimension(:)
  - `matmul__i32rank2_i32rank2` - integer(4),dimension(:,:) matmul

**Keep specifics private, export only generic:**
```fortran
module m_add_inst
  use m_add, only: add
  implicit none
  private
  public :: add   ! Only generic is public

  interface add
    module procedure add__r64_r64   ! Private specific
  end interface
contains
  real(8) function add__r64_r64(a, b)
    real(8), intent(in) :: a, b
    add__r64_r64 = a + b
  end function
end module
```

### Build and Cache Discipline

**Immutable library objects:**
- Never rewrite library `.o` or `.mod` files
- Library compilation happens once

**Caller artifacts:**
- Instantiation modules compile to new `.o`/`.mod` files
- Names are content-addressed: `<lib>_inst_<hash>.o`
- Change in call patterns → new caller object, library unchanged

**Fortfront workflow:**
1. User compiles library with fortfront → `lib.f90` → `lib.mod`, `lib.o`
2. User writes caller using library functions
3. Fortfront analyzes caller, detects needed specializations
4. Fortfront generates `lib_inst_caller.f90` with new specializations
5. Compile: `lib.o` + `lib_inst_caller.f90` → `caller.exe`
6. No mutation of `lib.o` or `lib.mod`

### Language Precedents

This approach combines features from multiple languages:

**1. Haskell Type Classes** - Extensible interfaces
- Orphan instances allow adding implementations in different modules
- Resolution at call site based on types
- No library mutation

**2. Julia Multiple Dispatch** - Cross-module method addition
- Methods can be added to existing functions from any module
- Dynamic dispatch at runtime; fortfront would be static

**3. Rust Traits** - Static resolution with coherence
- Trait implementations specialize at compile-time
- Similar to Rust but more permissive (no orphan rules)

**4. Swift Protocol Extensions** - Protocol extended in caller scope
- Extensions add conformance in different modules
- Static dispatch where possible

**Novel combination for Fortfront:**
- ✅ Haskell's extensibility (orphan instances)
- ✅ Julia's cross-module addition (multiple dispatch)
- ✅ Rust's zero-cost (compile-time monomorphization)
- ✅ 100% standard Fortran (no language extensions)

### Implementation Roadmap

**Phase 1: Single-File Monomorphization**
1. Enhance call graph to track unique type signatures per function
2. Modify codegen to generate multiple specifics when needed
3. Emit generic interface binding all specifics
4. Add `--monomorphize` flag (opt-in)

**Phase 2: Module Support**
1. Add `--emit-module` flag to generate library modules
2. Implement deterministic name mangling
3. Generate module with generic interface

**Phase 3: Caller-Side Instantiation**
1. Add library import analysis to fortfront
2. Detect when caller uses library function with new types
3. Generate instantiation module automatically
4. Emit build instructions for linking

**Phase 4: Optimization**
1. Ship prebuilt specializations for common numeric types
2. Add specialization count limits and warnings
3. Provide compilation cache for instantiation modules

### Limitations and Mitigations

**Limitation 1: Single-file analysis only**
- **Mitigation:** Module pattern allows post-hoc specialization
- Caller adds needed types without touching library

**Limitation 2: Literal types only**
- **Mitigation:** Type hints or explicit annotations for non-literal cases
- Consider `! fortfront: type(real(8)) :: x` pragma

**Limitation 3: Code bloat**
- **Mitigation:** Default cap at 5-10 specializations per function
- Warn user and suggest refactoring or manual approach

**Limitation 4: No type constraints**
- **Mitigation:** Runtime errors caught by Fortran compiler
- Future: add constraint syntax (requires language extension)

### Testing Strategy

**Unit tests:**
- Single-file monomorphization with 2-5 type combinations
- Interface generation correctness
- Name mangling uniqueness

**Integration tests:**
- Library + caller compilation
- Generic resolution at use site
- No library object mutation

**Regression tests:**
- Existing lazy fortran examples continue to work
- Default behavior unchanged (opt-in only)

## Conclusion

**Current fortfront:** Type inference from single call site, one specialization per function. This is sound for single-file numerical scripts.

**Proposed enhancement:** Usage-driven monomorphization using Fortran's native generic interface extension mechanism.

**Is it worth it?**
- **For single-file scripts:** Moderate benefit (multiple type uses in one file)
- **For library development:** High benefit (caller-side specialization)
- **For teaching:** Medium complexity (opt-in keeps it simple)
- **For performance:** Zero runtime overhead (static dispatch)

**Recommended path:**
1. **Phase 1 (high value):** Single-file monomorphization with `--monomorphize` flag
2. **Phase 2 (medium value):** Module emission for library development
3. **Phase 3 (high value for power users):** Caller-side instantiation
4. **Phase 4 (polish):** Prebuilt specializations and caching

**Key advantages of this approach:**
- ✓ 100% standard Fortran (no language extensions)
- ✓ Immutable library objects (clean build model)
- ✓ Zero runtime overhead (static resolution)
- ✓ Incremental adoption (opt-in features)
- ✓ Leverages existing fortfront infrastructure

**The Fortran-native pattern is battle-tested:** This approach mirrors how production Fortran libraries handle generics today, adapted for automatic generation from lazy fortran.

**Fortfront would be unique:** Automatic compile-time monomorphization from inferred usage, emitting standard Fortran with no language extensions. No other tool does this.
