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

**Separation of Concerns:**

Fortfront's responsibility is **single-file monomorphization**. Cross-module specialization, dependency management, and object caching are handled by **package managers** (e.g., fpm, fortran-lang ecosystem tools).

**What Fortfront Does:**

**Pattern 1: Single-File Monomorphization (Default Behavior)**
- Analyze all call sites within single `.lf` file
- Generate multiple specializations for functions used with different types
- Emit generic interface binding all specializations
- Output complete program or module with all variants

**This is not optional - it's how type inference works correctly.**

Example:
```fortran
! Input: script.lf
function add(a, b)
    add = a + b
end function

x = add(5, 3)
y = add(2.5, 1.5)
```

Output (standard behavior):
```fortran
module auto_add
    implicit none
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface add
contains
    integer function add__i32_i32(a, b)
        integer, intent(in) :: a, b
        add__i32_i32 = a + b
    end function

    real(8) function add__r64_r64(a, b)
        real(8), intent(in) :: a, b
        add__r64_r64 = a + b
    end function
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real(8) :: y
    x = add(5, 3)
    y = add(2.5, 1.5)
end program
```

**What Package Managers Do:**

**Pattern 2: Cross-Module Specialization (Package Manager Responsibility)**
- Global dependency resolution
- `.mod` and `.o` file caching across packages
- Caller-side instantiation module generation
- Build orchestration

Package manager workflow:
1. User writes library.lf → fortfront generates library module
2. User writes caller.lf that uses library
3. Package manager detects library dependency
4. Package manager analyzes caller usage + library interface
5. Package manager generates instantiation module if needed
6. Package manager orchestrates compilation with cache

**Pattern 3: Prebuilt Specializations (Library Author Choice)**
- Library ships multiple common specializations upfront
- Reduces instantiation overhead for common cases
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

### Infrastructure Fortfront Provides for Package Managers

Package managers use **fortfront's library API** to access type information and generate code.

**1. API: Get Type Signatures and Specializations**

```fortran
use fortfront_semantic, only: get_function_specializations
use fortfront_types, only: specialization_info_t

type(specialization_info_t), allocatable :: specs(:)
call get_function_specializations(arena, function_name, specs)

! specs contains:
! - signature (type info for each parameter)
! - mangled_name (e.g., "add__i32_i32")
! - return_type
```

**2. API: Get Call Site Analysis**

```fortran
use fortfront_semantic, only: get_call_sites
use fortfront_types, only: call_site_info_t

type(call_site_info_t), allocatable :: calls(:)
call get_call_sites(arena, function_name, calls)

! calls contains:
! - location (line, column)
! - argument_types
! - node_index (for AST access)
```

**3. API: Generate Instantiation Module**

```fortran
use fortfront_codegen, only: generate_instantiation_module

character(len=:), allocatable :: fortran_code
call generate_instantiation_module(base_module_name, function_name, &
                                   signature, fortran_code)

! fortran_code contains complete Fortran module with:
! - use statement for base module
! - interface extension
! - specialized implementation
```

**4. CLI (for human users, not package managers)**

```bash
# Standard transformation (monomorphization is automatic)
fortfront input.lf -o output.f90
```

Package managers call fortfront API functions directly from their own code.

### Build and Cache Discipline (Package Manager Responsibility)

**Immutable library objects:**
- Never rewrite library `.o` or `.mod` files
- Library compilation happens once
- Package manager caches in global store

**Caller artifacts:**
- Instantiation modules compile to new `.o`/`.mod` files
- Names are content-addressed: `<lib>_inst_<hash>.o`
- Package manager caches these too
- Change in call patterns → new caller object, library unchanged

**Package manager workflow:**
1. User writes library.lf
2. Package manager calls fortfront API to transform lib.lf
3. Package manager queries specializations via API: `get_function_specializations()`
4. Package manager caches `lib.mod`, `lib.o`, specialization metadata
5. User writes caller.lf using library
6. Package manager calls fortfront API to analyze caller.lf
7. Package manager queries call sites via API: `get_call_sites()`
8. Package manager compares caller needs vs library provides
9. If gap exists, package manager calls API: `generate_instantiation_module()`
10. Package manager compiles instantiation module
11. Package manager orchestrates build: `lib.o` + `inst.o` + `caller.o` → `exe`
12. All artifacts cached globally by package manager

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

**Fortfront Core (Single-File Focus):**

**Phase 1: Single-File Monomorphization (PRIORITY)**
1. Enhance call graph to track unique type signatures per function
2. Modify codegen to generate multiple specifics when needed
3. Emit generic interface binding all specifics
4. **Goal:** Complete monomorphization within single `.lf` file (standard behavior)

**Phase 2: API for Package Managers**
1. Add `get_function_specializations()` API to query type signatures
2. Add `get_call_sites()` API to analyze function usage
3. Add `generate_instantiation_module()` API to create specialization code
4. Design API types for package manager consumption
5. **Goal:** Enable package managers to orchestrate cross-module specialization via API

**Phase 3: Module Emission**
1. Add `--emit-module` flag to generate library modules (instead of programs)
2. Implement deterministic name mangling
3. Generate module with generic interface
4. **Goal:** Libraries can be compiled with fortfront and used by package managers

**Package Manager Responsibilities (NOT in fortfront):**

**Package managers (fpm, etc.) will implement:**
1. Global `.mod` and `.o` file caching
2. Dependency resolution across packages
3. Caller-side instantiation orchestration
4. Build graph management
5. Incremental compilation based on content hashes
6. Artifact sharing across projects

**Clear division:**
- **Fortfront**: Transform single `.lf` file, provide metadata/templates
- **Package Manager**: Orchestrate multi-file builds, manage cache, resolve dependencies

### Limitations and Mitigations

**Limitation 1: Single-file analysis in fortfront**
- **Mitigation:** Package managers handle cross-module specialization
- Fortfront provides metadata and infrastructure
- Clean separation of concerns

**Limitation 2: Literal types only (in fortfront)**
- **Mitigation:** Type hints or explicit annotations for non-literal cases
- Consider `! fortfront: type(real(8)) :: x` pragma
- Package managers can use module interface information

**Limitation 3: Code bloat**
- **Mitigation:** Default cap at 5-10 specializations per function in single file
- Package managers can use smarter strategies (lazy instantiation, LTO)
- Warn user and suggest refactoring or manual approach

**Limitation 4: No type constraints**
- **Mitigation:** Type errors caught by Fortran compiler during codegen
- Clear error messages guide user
- Future: add constraint syntax (requires language extension)

### Testing Strategy

**Fortfront Unit Tests:**
- Single-file monomorphization with 2-5 type combinations
- Interface generation correctness
- Name mangling uniqueness
- Metadata export format validation
- Instantiation template generation

**Fortfront Integration Tests:**
- Complete single-file workflows (input.lf → output.f90 → compile)
- Metadata roundtrip (emit → parse → verify)
- Module emission correctness

**Package Manager Tests (outside fortfront):**
- Library + caller compilation
- Generic resolution at use site
- No library object mutation
- Global cache functionality
- Incremental compilation

**Regression Tests:**
- Existing lazy fortran examples continue to work
- Single-type functions remain simple (no interface overhead)
- No performance degradation for simple cases

## Conclusion

**Current fortfront:** Type inference from single call site, one specialization per function. This is sound for single-file numerical scripts.

**Proposed enhancement:** Usage-driven monomorphization using Fortran's native generic interface extension mechanism.

### Separation of Concerns

**Fortfront's Scope (IMPLEMENTED):**
- ✅ Single-file monomorphization (complete solution)
- ✅ Metadata export for package managers
- ✅ Instantiation template generation
- ✅ Module interface queries

**Package Manager's Scope (EXTERNAL):**
- ✅ Cross-module dependency resolution
- ✅ Global `.mod` and `.o` caching
- ✅ Caller-side instantiation orchestration
- ✅ Build graph management
- ✅ Incremental compilation

**This clean separation enables:**
- Fortfront stays focused and maintainable
- Package managers leverage their expertise (caching, dependencies)
- Users get complete solution through ecosystem

### Value Proposition

**For single-file scripts (fortfront handles completely):**
- ✓ **High value:** Multiple type uses in one file work correctly
- ✓ **Zero overhead:** Static dispatch, no runtime cost
- ✓ **Simple:** Just works automatically

**For library development (fortfront + package manager):**
- ✓ **High value:** Caller-side specialization without library mutation
- ✓ **Scalable:** Package manager caches artifacts globally
- ✓ **Standard:** 100% Fortran, no language extensions

**For teaching:**
- ✓ **Low complexity:** Single-file case is straightforward
- ✓ **No surprises:** Multiple type uses just work
- ✓ **Progressive:** Learn simple case first, ecosystem later

### Recommended Implementation Path

**Phase 1 (HIGH PRIORITY - Fortfront):**
Single-file monomorphization (standard behavior)
- **Value:** Complete solution for scripts
- **Effort:** Medium (use existing call graph, codegen)
- **Risk:** Low (natural extension of type inference)

**Phase 2 (MEDIUM PRIORITY - Fortfront):**
API for package managers (query functions, instantiation generation)
- **Value:** Enables ecosystem tools
- **Effort:** Low (expose existing data structures via API)
- **Risk:** Low (package managers call API, fortfront just provides)

**Phase 3 (LOW PRIORITY - Fortfront):**
Module emission and interface queries
- **Value:** Library authoring support
- **Effort:** Low (minor codegen changes)
- **Risk:** Low (standard Fortran modules)

**Phase 4 (EXTERNAL - Package Managers):**
Cross-module orchestration, caching, dependency resolution
- **Value:** Complete ecosystem solution
- **Effort:** High (package manager domain)
- **Risk:** Medium (complex build systems)

### Key Advantages

- ✓ **100% standard Fortran** (no language extensions)
- ✓ **Immutable library objects** (clean build model)
- ✓ **Zero runtime overhead** (static resolution)
- ✓ **Natural behavior** (completes type inference correctly)
- ✓ **Leverages existing infrastructure** (call graph, codegen, interfaces)
- ✓ **Clean separation of concerns** (fortfront = transformation, packages = orchestration)

### Why This Matters

**The Fortran-native pattern is battle-tested:** This approach mirrors how production Fortran libraries handle generics today, adapted for automatic generation from lazy fortran.

**Fortfront would be unique:** Automatic compile-time monomorphization from inferred usage, emitting standard Fortran with no language extensions. No other tool does this.

**Ecosystem integration:** By providing infrastructure hooks, fortfront enables package managers to deliver complete cross-module solutions while staying focused on its core mission: transforming lazy Fortran.

**Users win:** Simple scripts "just work" with fortfront alone. Complex multi-package projects get full monomorphization through package manager integration. Both use the same underlying mechanism.
