# fortfront Architecture Design

**FPM-FIRST ARCHITECTURE**
- **Dependency Management**: FPM automatically handles all tool dependencies via fpm.toml
- **Pure Fortran Interface**: External tools integrate through standard Fortran modules
- **Simple Integration**: Tools add `fortfront` as dependency - FPM handles the rest
- **No Build Complexity**: FPM manages module compilation and linking automatically

## Table of Contents
1. [Foundation Architecture (MANDATORY)](#foundation-architecture-mandatory)
2. [Static Linking Strategy](#static-linking-strategy)
3. [Compilation Pipeline](#compilation-pipeline)
4. [Arena Memory Architecture](#arena-memory-architecture)
5. [Dual Tree Architecture (CST/AST)](#dual-tree-architecture-cstast)
6. [Type System (Hindley-Milner)](#type-system-hindley-milner)
7. [AST Patching (Lazy → Standard)](#ast-patching-lazy--standard)
8. [Fortran Module Interfaces for External Tools](#fortran-module-interfaces-for-external-tools)
9. [Implementation Roadmap](#implementation-roadmap)
10. [Input Validation Module Architecture](#input-validation-module-architecture)

---

# Foundation Architecture (FPM-FIRST)

## Core Mission

fortfront is a **Fortran frontend library** that ALL Fortran tooling builds upon through FPM's automatic dependency management.

## Build Requirements (FPM-MANAGED)

```toml
# FPM handles all dependency management
# External tools simply add fortfront as dependency:
[dependencies]
fortfront = { path = "../fortfront" }
# FPM automatically compiles and links all modules
```

## Dependency Architecture

```
fortfront (FPM library)
    ↑ FPM dependency management
    ├── fluff (FPM handles linking)
    ├── ffc (FPM handles linking) 
    ├── fortnb (FPM handles linking)
    ├── fortcov (FPM handles linking)
    ├── fortrun (FPM handles linking)
    └── fo (FPM handles linking)
```

**SIMPLICITY**: FPM automatically manages all dependencies. No manual build complexity required.

## FPM Integration Strategy

### Why FPM is the Right Choice

1. **Automatic Dependency Management**: FPM handles all module compilation and linking
2. **Standard Fortran Practice**: Industry-standard dependency management
3. **Zero Configuration**: External tools just add one line to fmp.toml
4. **Maintainable**: No complex build scripts or manual library management
5. **Reliable**: FPM's proven dependency resolution

### Project Architecture

```
fortfront/
├── src/                    # All fortfront source code
├── fpm.toml               # Standard FPM library configuration
└── Public API modules exposed automatically by FPM
```

### Tool Integration Pattern

**Every external tool follows this simple pattern:**

```fortran
! Example: fluff main program
program fluff_main
    use frontend              ! Use existing fortfront modules
    use ast_core             ! FPM makes all modules available
    use semantic_analyzer    ! Direct access to functionality
    
    type(compiler_state_t) :: state
    logical :: success
    
    success = compile_source("source.f90", state)
    call analyze_for_lint_issues(state%ast)
end program
```

```toml
# fluff/fmp.toml
[dependencies]
fortfront = { path = "../fortfront" }
# Result: FPM automatically handles all module compilation and linking
```

## Key Architectural Components

### 1. Arena-Based Memory Management
- **Core**: Arena allocator with `(id, generation)` handles for safety
- **Stable IDs**: Each node gets a permanent `uid:int64` for external tool integration
- **Performance**: 10-100x improvement over malloc/free patterns
- **Safety**: Generation-based validation prevents use-after-free

### 2. Dual Tree Architecture
- **CST (Concrete Syntax Tree)**: Preserves all source information (comments, whitespace, formatting)
- **AST (Abstract Syntax Tree)**: Normalized representation for semantic analysis
- **Bidirectional Links**: CST ↔ AST mapping for source reconstruction

### 3. Type System
- **Algorithm**: Hindley-Milner type inference with constraint solving
- **Unification**: Constraint-based type unification with occurs check
- **Polymorphism**: Support for generic types and type variables
- **Integration**: Type arena for efficient type storage

### 4. AST Patching
- **Lazy Fortran**: Support for minimal syntax (`x = 2` without declaration)
- **Materialization**: Insert implicit declarations into AST
- **Transformation**: Convert lazy Fortran to standard Fortran
- **Preservation**: Maintain original source structure where possible

### 5. Plugin Architecture
- **External Tools**: Clean API for fluff (linting), ffc (compilation), fortrun (execution)
- **Event System**: Hooks for analysis phases
- **Extensibility**: Custom analyzers without modifying core

## Current Implementation Status

### ✅ What We Already Have

1. **Arena Infrastructure**
   - `src/memory/arena_memory.f90` - Base arena allocator
   - `src/ast/ast_arena_modern.f90` - AST arena with generations
   - `src/semantic/type_system_arena.f90` - Type system arena
   - `src/memory/compiler_arena.f90` - Unified coordinator (unused)

2. **AST System**
   - Complete AST node hierarchy in `src/ast/ast_nodes_*.f90`
   - Arena-based storage (using deprecated `ast_arena.f90`)
   - Comment nodes for preserving source information

3. **Type System**
   - `type_system_unified.f90` - Arena-backed type system
   - Basic type inference in `semantic_analyzer.f90`
   - Type variables and monomorphic types

4. **Semantic Pipeline**
   - `semantic_pipeline.f90` - Extensible analyzer framework
   - Built-in analyzers for various semantic checks
   - Plugin infrastructure for external tools

### ❌ What We Need to Add

1. **CST Implementation**
   - Create concrete syntax tree that preserves all trivia
   - Maintain bidirectional CST ↔ AST links
   - Source reconstruction from CST

2. **Stable UIDs**
   - Add `uid:int64` field to all AST nodes
   - UID generation and management
   - External tool API using UIDs

3. **Hindley-Milner Completion**
   - Full constraint generation
   - Constraint solver with occurs check
   - Let-polymorphism support

4. **AST Patching System**
   - Implicit declaration detection
   - Declaration materialization
   - Lazy → Standard transformation

5. **Arena Unification**
   - Migrate from `ast_arena.f90` to `ast_arena_modern.f90`
   - Activate `compiler_arena.f90` as coordinator
   - Generation management between phases

---

# Compilation Pipeline

## Phase Overview

```
Source Code
    ↓
[Lexer] → Tokens (with trivia)
    ↓
[Parser] → CST (lossless)
    ↓
[CST→AST] → AST (normalized) + CST links
    ↓
[Type Inference] → Typed AST (Hindley-Milner)
    ↓
[AST Patching] → Standard AST (declarations inserted)
    ↓
[Code Generation] → Standard Fortran
```

## Phase Details

### 1. Lexical Analysis
- **Input**: Source text
- **Output**: Token stream with position information
- **Preserves**: Comments, whitespace (as trivia tokens)
- **Module**: `src/lexer/lexer_core.f90`

### 2. Parsing
- **Input**: Token stream
- **Output**: CST (Concrete Syntax Tree)
- **Preserves**: All source structure, formatting, comments
- **Module**: `src/parser/parser_core.f90`
- **TODO**: Create CST nodes that preserve trivia

### 3. CST to AST Conversion
- **Input**: CST
- **Output**: AST with CST links
- **Creates**: Normalized representation
- **Maintains**: Bidirectional mapping CST ↔ AST
- **TODO**: Implement CST→AST converter

### 4. Type Inference
- **Input**: Untyped AST
- **Output**: Typed AST with `inferred_type` fields
- **Algorithm**: Hindley-Milner with constraint solving
- **Module**: `src/semantic/semantic_analyzer.f90`
- **TODO**: Complete constraint solver

### 5. AST Patching
- **Input**: Typed AST (possibly lazy)
- **Output**: Standard-compliant AST
- **Inserts**: Implicit declarations for undeclared variables
- **Preserves**: Original structure where possible
- **TODO**: Implement declaration materializer

### 6. Code Generation
- **Input**: Standard AST
- **Output**: Fortran source code
- **Module**: `src/codegen/codegen_core.f90`
- **Options**: Use CST for original formatting or AST for normalized

---

# Arena Memory Architecture

## Design Goals
- **Performance**: O(1) allocation, bulk deallocation
- **Safety**: Generation-based handle validation
- **Efficiency**: Cache-optimal sequential layout
- **Simplicity**: No manual memory management

## Arena Hierarchy

```
compiler_arena_t (Coordinator)
    ├── ast_arena_t (AST nodes)
    ├── type_arena_t (Type system)
    ├── symbol_arena_t (Symbols) [TODO]
    └── literal_arena_t (String pool) [TODO]
```

## Handle Structure

```fortran
type :: arena_handle_t
    integer :: id         ! Unique identifier within arena
    integer :: generation ! Generation for validity checking
    integer :: offset     ! Byte offset in chunk
    integer :: size       ! Allocation size
end type
```

## Unified API

All arenas implement this interface:

```fortran
type, abstract :: base_arena_t
contains
    procedure(insert_interface), deferred :: insert  ! Add item
    procedure(get_interface), deferred :: get       ! Retrieve item
    procedure(valid_interface), deferred :: valid   ! Check handle
    procedure(free_interface), deferred :: free     ! Mark as free
    procedure :: reset        ! Bulk deallocation
    procedure :: checkpoint   ! Save state
    procedure :: rollback     ! Restore state
end type
```

## Migration Plan

### Current State
- **36 files** use deprecated `ast_arena.f90`
- **1 file** uses `compiler_arena.f90` (test only)
- **9 files** use `type_system_arena.f90`

### Target State
- All files use unified arena API
- `compiler_arena.f90` coordinates all arenas
- Generation increments between compilation phases
- External tools access via stable UIDs

---

# Dual Tree Architecture (CST/AST)

## Overview

The dual tree architecture separates source representation concerns:
- **CST (Concrete Syntax Tree)**: Preserves ALL source information for round-tripping
- **AST (Abstract Syntax Tree)**: Contains only semantic information for analysis

This separation enables:
- Exact source reconstruction for formatting tools
- Clean semantic analysis without formatting concerns
- Precise error messages with original source context
- Support for both compiler and formatter use cases

## Concrete Syntax Tree (CST)

### Purpose
- Preserve **all** source information
- Support exact source reconstruction
- Enable refactoring tools
- Maintain formatting for code generation

### CST Node Structure

```fortran
type :: cst_node_t
    integer(int64) :: uid       ! Stable unique identifier
    integer :: kind             ! Node type (CST_* constants)
    integer :: start_pos        ! Source position start
    integer :: end_pos          ! Source position end
    
    ! Arena handles for tree structure
    integer :: ast_link         ! Index to corresponding AST node
    integer, allocatable :: children(:)     ! Child CST nodes
    integer, allocatable :: trivia_before(:) ! Leading trivia
    integer, allocatable :: trivia_after(:)  ! Trailing trivia
    
    ! Direct text for terminals
    character(len=:), allocatable :: text  ! For identifiers, literals, keywords
end type

type :: cst_trivia_t
    integer :: kind             ! TRIVIA_COMMENT, TRIVIA_WHITESPACE, TRIVIA_NEWLINE
    integer :: start_pos
    integer :: end_pos
    character(len=:), allocatable :: text
end type
```

### CST Node Types

```fortran
! CST node kinds (more granular than AST)
integer, parameter :: CST_PROGRAM = 1
integer, parameter :: CST_FUNCTION = 2
integer, parameter :: CST_SUBROUTINE = 3
integer, parameter :: CST_ASSIGNMENT = 4
integer, parameter :: CST_BINARY_OP = 5
integer, parameter :: CST_IDENTIFIER = 6
integer, parameter :: CST_LITERAL = 7
integer, parameter :: CST_KEYWORD = 8
integer, parameter :: CST_OPERATOR = 9
integer, parameter :: CST_DELIMITER = 10  ! Parentheses, commas
integer, parameter :: CST_DECLARATION = 11
! ... more CST types

! Trivia kinds
integer, parameter :: TRIVIA_COMMENT = 1
integer, parameter :: TRIVIA_WHITESPACE = 2
integer, parameter :: TRIVIA_NEWLINE = 3
integer, parameter :: TRIVIA_CONTINUATION = 4  ! Line continuation &
```

### Implementation Status
- ❌ CST node types definition
- ❌ CST arena allocation
- ❌ Lexer trivia preservation
- ❌ Parser CST construction
- ❌ CST→AST converter
- ❌ Source reconstruction

## Abstract Syntax Tree (AST)

### Current Implementation
- Complete node hierarchy in `src/ast/ast_nodes_*.f90`
- Arena-based storage (needs migration)
- Type information via `inferred_type` field
- Comment nodes mixed with semantic nodes

### Enhanced AST Node

```fortran
type :: ast_node
    integer(int64) :: uid       ! Stable identifier for external tools
    integer :: cst_origin       ! Index to originating CST node
    type(mono_type_t) :: inferred_type
    
    ! Existing position info (derived from CST)
    integer :: line
    integer :: column
    
    ! Existing semantic info
    logical :: is_constant
    ! ... other fields ...
end type
```

### Key Changes from Current
- Remove comment nodes from AST (move to CST)
- Add uid field for stable external references
- Add cst_origin for error reporting
- Keep semantic-only information

## Bidirectional Mapping

### CST → AST
- Each CST node optionally links to corresponding AST node
- Multiple CST nodes may map to single AST node
- Trivia nodes have no AST correspondence
- Keywords and delimiters have no AST nodes

### AST → CST
- Each AST node links back to originating CST node
- Used for error reporting with exact positions
- Enables source-preserving transformations

### Mapping Examples

```fortran
! Source
x = 2 + 3  ! comment

! CST Structure
CST_ASSIGNMENT
├── CST_IDENTIFIER "x"
├── CST_OPERATOR "="
├── CST_BINARY_OP
│   ├── CST_LITERAL "2"
│   ├── CST_OPERATOR "+"
│   └── CST_LITERAL "3"
└── trivia_after: TRIVIA_COMMENT "! comment"

! AST Structure (cleaner)
assignment_node
├── identifier_node "x"
└── binary_op_node "+"
    ├── literal_node 2
    └── literal_node 3
```

## Migration Strategy

### Phase 1: Parallel Implementation (Keep System Working)
1. Create CST infrastructure alongside existing AST
2. Build CST in parser while still building AST
3. Test CST construction without breaking existing flow

### Phase 2: Integration
1. Create CST→AST converter
2. Switch parser to CST-first approach
3. Generate AST from CST
4. Verify all tests pass

### Phase 3: Cleanup
1. Remove direct AST construction from parser
2. Move comment handling to CST only
3. Add source reconstruction from CST
4. Update external APIs

---

# Type System (Hindley-Milner)

## Overview

fortfront implements Hindley-Milner type inference with extensions for Fortran-specific features.

## Type Representation

### Current Implementation

```fortran
! Type kinds (in type_system_unified.f90)
integer, parameter :: TVAR = 1      ! Type variable
integer, parameter :: TINT = 2      ! Integer
integer, parameter :: TREAL = 3     ! Real
integer, parameter :: TCHAR = 4     ! Character
integer, parameter :: TLOGICAL = 5  ! Logical
integer, parameter :: TFUN = 6      ! Function
integer, parameter :: TARRAY = 7    ! Array

! Monomorphic type
type :: mono_type_t
    type(mono_handle_t) :: handle
    integer :: kind
    type(type_var_t) :: var
    ! ... more fields ...
end type

! Polymorphic type scheme
type :: poly_type_t
    type(poly_handle_t) :: handle
    ! ∀α₁...αₙ. τ
end type
```

## Type Inference Algorithm

### Current State
- Basic type inference in `semantic_analyzer.f90`
- Simplified unification without full constraint solving
- No let-polymorphism

### TODO: Complete Implementation

#### 1. Constraint Generation

```fortran
type :: constraint_t
    integer :: kind  ! EQUALS, INSTANCE_OF, etc.
    type(mono_type_t) :: left
    type(mono_type_t) :: right
end type

type :: constraint_set_t
    type(constraint_t), allocatable :: constraints(:)
    integer :: count
end type
```

#### 2. Constraint Solving

```fortran
! Generate constraints from AST
function generate_constraints(ast, env) result(constraints)
    ! Walk AST generating type constraints
    ! x = 2 generates: type(x) = INTEGER
    ! x + y generates: type(x) = type(y) = NUMBER
end function

! Solve constraint set
function solve_constraints(constraints) result(substitution)
    ! Unification algorithm
    ! Occurs check for infinite types
    ! Build substitution map
end function
```

#### 3. Type Application

```fortran
! Apply substitution to AST
subroutine apply_types(ast, substitution)
    ! Set inferred_type field on all nodes
    ! Propagate types through tree
end subroutine
```

## Fortran Extensions

### Array Types
- Shape inference
- Rank compatibility
- Assumed-shape/deferred-shape handling

### Character Types
- Length inference
- Assumed-length parameters
- Concatenation type rules

### Implicit Typing
- Support Fortran implicit typing rules
- Override with explicit declarations
- Handle `implicit none`

---

# AST Patching (Lazy → Standard)

## Purpose

Transform lazy Fortran (minimal syntax) into standard Fortran by materializing implicit declarations.

## Lazy Fortran Features

### Implicit Variable Declaration
```fortran
! Lazy Fortran (input)
x = 2
y = 3.14
name = "Alice"

! Standard Fortran (output after patching)
integer :: x
real :: y
character(len=5) :: name
x = 2
y = 3.14
name = "Alice"
```

### Implicit Program Structure
```fortran
! Lazy Fortran (input)
print *, "Hello"

! Standard Fortran (output)
program main
    implicit none
    print *, "Hello"
end program main
```

## Implementation Strategy

### Phase 1: Type Inference
- Run Hindley-Milner inference
- Each undeclared variable gets inferred type
- Build declaration requirements

### Phase 2: Declaration Materialization

```fortran
type :: declaration_materializer_t
contains
    procedure :: materialize
    procedure :: find_undeclared_vars
    procedure :: create_declaration_node
    procedure :: insert_declaration
end type

function materialize_declarations(ast) result(patched_ast)
    ! 1. Find all undeclared variables
    ! 2. Group by type for efficient declarations
    ! 3. Create declaration nodes
    ! 4. Insert at appropriate scope level
end function
```

### Phase 3: Program Structure
- Add `program main` if missing
- Add `implicit none` if appropriate
- Add `end program` statement

## TODO: Implementation

1. Create `src/transform/lazy_to_standard.f90`
2. Implement declaration materializer
3. Add program structure normalizer
4. Integrate into compilation pipeline
5. Add configuration option for lazy mode

---

# Fortran Module Interfaces for External Tools

## Overview

fortfront provides clean Fortran module interfaces that enable external tools to integrate with the Fortran frontend using standard Fortran dependency mechanisms through fpm.

## Design Principles

1. **Pure Fortran**: All interfaces use standard Fortran types and procedures
2. **Type Safety**: Full Fortran type checking across tool boundaries
3. **Stable Interface**: Module interfaces remain stable across fortfront versions
4. **Handle-Based**: Safe integer handles for AST nodes with generation checking
5. **Zero Dependencies**: Integration requires only standard Fortran

## Core Module Interfaces

### Main Compilation Interface (`fortfront_core.f90`)

```fortran
module fortfront_core
    use iso_fortran_env, only: int64
    implicit none
    private
    
    ! Public compilation interface
    public :: fortfront_compile_source, fortfront_compile_file
    public :: fortfront_get_ast_root, fortfront_get_errors
    public :: fortfront_context_t, fortfront_result_t
    
    ! Context type for compilation state
    type :: fortfront_context_t
        private
        integer(int64) :: handle
    end type
    
    ! Result type for error handling
    type :: fortfront_result_t
        logical :: success
        character(len=:), allocatable :: message
        character(len=:), allocatable :: suggestion
        integer :: line, column
    end type
    
    interface
        function fortfront_compile_source(source_text, ast_handle) result(result)
            character(len=*), intent(in) :: source_text
            integer(int64), intent(out) :: ast_handle
            type(fortfront_result_t) :: result
        end function
        
        function fortfront_compile_file(filename, ast_handle) result(result)
            character(len=*), intent(in) :: filename
            integer(int64), intent(out) :: ast_handle
            type(fortfront_result_t) :: result
        end function
    end interface
end module
```

### AST Access Interface (`fortfront_ast.f90`)

```fortran
module fortfront_ast
    use iso_fortran_env, only: int64
    use fortfront_core, only: fortfront_result_t
    implicit none
    private
    
    ! Public AST traversal interface
    public :: fortfront_get_node_type, fortfront_get_node_text
    public :: fortfront_get_children, fortfront_get_parent
    public :: fortfront_get_node_position
    
    interface
        function fortfront_get_node_type(node_handle) result(node_type)
            integer(int64), intent(in) :: node_handle
            integer :: node_type
        end function
        
        function fortfront_get_node_text(node_handle) result(text)
            integer(int64), intent(in) :: node_handle
            character(len=:), allocatable :: text
        end function
        
        function fortfront_get_children(node_handle, child_handles) result(count)
            integer(int64), intent(in) :: node_handle
            integer(int64), allocatable, intent(out) :: child_handles(:)
            integer :: count
        end function
    end interface
end module
```

### Semantic Analysis Interface (`fortfront_semantic.f90`)

```fortran
module fortfront_semantic
    use iso_fortran_env, only: int64
    use fortfront_core, only: fortfront_result_t
    implicit none
    private
    
    ! Public semantic analysis interface
    public :: fortfront_get_type_info, fortfront_get_scope_info
    public :: fortfront_run_semantic_analysis
    public :: fortfront_type_info_t, fortfront_scope_info_t
    
    ! Type information
    type :: fortfront_type_info_t
        character(len=:), allocatable :: type_name
        integer :: type_kind
        logical :: is_polymorphic
    end type
    
    interface
        function fortfront_get_type_info(node_handle) result(type_info)
            integer(int64), intent(in) :: node_handle
            type(fortfront_type_info_t) :: type_info
        end function
    end interface
end module
```

## Tool Integration Examples

### fluff (Linter)
```fortran
program fluff_main
    use fortfront_core
    use fortfront_ast
    use fortfront_semantic
    
    integer(int64) :: ast_handle
    type(fortfront_result_t) :: result
    
    result = fortfront_compile_file("source.f90", ast_handle)
    if (result%success) then
        call analyze_for_lint_issues(ast_handle)
    else
        print *, "Parse error:", result%message
    end if
end program
```

### ffc (Compiler)
```fortran
program ffc_main
    use fortfront_core
    use fortfront_semantic
    
    integer(int64) :: ast_handle
    type(fortfront_result_t) :: result
    
    result = fortfront_compile_source(source_text, ast_handle)
    if (result%success) then
        call compile_to_llvm(ast_handle)
    end if
end program
```

### fortrun (Runner)
```fortran
program fortrun_main
    use fortfront_core
    use fortfront_ast
    
    integer(int64) :: ast_handle
    
    result = fortfront_compile_file("main.lf", ast_handle)
    call discover_dependencies(ast_handle)
    call manage_build_cache(ast_handle)
end program
```

## Integration via fmp

### Tool fmp.toml Pattern
```toml
# External tool fmp.toml
name = "fluff"
version = "0.1.0"

[dependencies]
fortfront = { path = "../fortfront" }

[[executable]]
name = "fluff"
source-dir = "app"
main = "main.f90"
```

### Build Process
```bash
# Tool builds automatically with fortfront integration
cd fluff/
fmp build --profile release  # Static links libfortfront.a
# Result: Single executable with zero external dependencies
```

## Module Organization

- `fortfront_core` - Main compilation pipeline and context management
- `fortfront_ast` - AST node access, traversal, and manipulation
- `fortfront_semantic` - Type system and semantic analysis information
- `fortfront_memory` - Arena management interfaces (if needed by tools)
- `fortfront_errors` - Enhanced error handling and reporting

---

# CST/AST Split Implementation Plan

## Overview
This section details the complete migration from the current combined AST to separate CST and AST structures, maintaining system functionality at each step.

## Migration Phases

### Phase 0: Foundation (Prerequisites)
**Goal**: Prepare infrastructure without breaking existing code

#### Tasks:
1. **Create CST Module Structure**
   - Create `src/cst/` directory
   - Define `cst_nodes.f90` with node types
   - Define `cst_arena.f90` for CST storage
   - Define `cst_trivia.f90` for trivia handling

2. **Add UID Infrastructure**
   - Add `uid_generator.f90` module
   - Generate unique int64 IDs for all nodes
   - Create UID lookup tables

3. **Extend Lexer for Trivia**
   - Create `token_with_trivia_t` type
   - Collect whitespace and comments as trivia
   - Keep backward compatibility with current token stream

**Files to Create:**
- `src/cst/cst_nodes.f90`
- `src/cst/cst_arena.f90`
- `src/cst/cst_trivia.f90`
- `src/common/uid_generator.f90`
- `src/lexer/lexer_trivia.f90`

### Phase 1: Parallel CST Construction
**Goal**: Build CST alongside existing AST (system remains functional)

#### Tasks:
1. **Create CST Builder**
   - Implement `cst_builder.f90` that constructs CST from tokens
   - CST builder runs in parallel with existing parser
   - No changes to existing AST construction

2. **Add CST to Parser State**
   - Extend `parser_state_t` to include CST arena
   - Build CST nodes during parsing
   - Keep existing AST construction unchanged

3. **Implement CST Validation**
   - Create round-trip tests: source → CST → source
   - Verify CST captures all source information
   - Test trivia attachment

**Files to Modify:**
- `src/parser/parser_state.f90` - Add CST arena
- `src/parser/parser_dispatcher.f90` - Add CST construction calls

**Files to Create:**
- `src/cst/cst_builder.f90`
- `src/cst/cst_validator.f90`
- `test/cst/test_cst_roundtrip.f90`

### Phase 2: CST to AST Converter
**Goal**: Generate AST from CST instead of direct parsing

#### Tasks:
1. **Implement Converter**
   - Create `cst_to_ast.f90` converter module
   - Map CST nodes to AST nodes
   - Establish bidirectional links

2. **Add Converter Tests**
   - Test CST → AST conversion for all node types
   - Verify semantic equivalence
   - Check bidirectional links

3. **Create Dual-Path Parser**
   - Add parser option: `use_cst_path`
   - When enabled: Tokens → CST → AST
   - When disabled: Tokens → AST (current path)
   - Default to disabled (keep system working)

**Files to Create:**
- `src/cst/cst_to_ast.f90`
- `src/cst/cst_ast_mapper.f90`
- `test/cst/test_cst_to_ast.f90`

### Phase 3: Migration of Parser Modules
**Goal**: Switch each parser module to CST-first approach

#### Step-by-Step Module Migration:
Each module migrated independently to maintain system stability.

1. **Migrate Expression Parser** (`parser_expressions.f90`)
   - Switch to building CST nodes
   - Remove direct AST construction
   - Test thoroughly before proceeding

2. **Migrate Declaration Parser** (`parser_declarations.f90`)
   - Convert to CST construction
   - Handle type specifications in CST

3. **Migrate Control Flow Parser** (`parser_control_flow.f90`)
   - Convert if/do/select constructs

4. **Migrate Statement Parsers**
   - `parser_execution_statements.f90`
   - `parser_io_statements.f90`
   - `parser_memory_statements.f90`

5. **Migrate Definition Parser** (`parser_definition_statements.f90`)
   - Functions, subroutines, modules

**Migration Pattern for Each Module:**
```fortran
! Old pattern (remove)
function parse_expression(...) result(ast_node)
    ! Direct AST construction
end function

! New pattern (add)
function parse_expression_cst(...) result(cst_index)
    ! Build CST only
end function
```

### Phase 4: Cleanup and Optimization
**Goal**: Remove legacy code and optimize performance

#### Tasks:
1. **Remove Comment Nodes from AST**
   - Delete `comment_node` type from AST
   - Move all comment handling to CST
   - Update visitors to skip comment processing

2. **Remove Direct AST Construction**
   - Delete old parsing functions
   - Remove AST construction from parser
   - Clean up parser_dispatcher

3. **Optimize Memory Usage**
   - Unify arena allocation strategies
   - Share string pools between CST/AST
   - Implement arena compaction

4. **Update External APIs**
   - Modify codegen to use CST for formatting
   - Update semantic analyzer to ignore trivia
   - Enhance error messages with CST context

**Files to Modify:**
- All parser modules (remove old functions)
- `src/ast/ast_nodes_misc.f90` (remove comment_node)
- `src/codegen/codegen_core.f90` (use CST for output)

### Phase 5: Advanced Features
**Goal**: Leverage CST/AST split for new capabilities

#### Features:
1. **Source Reconstruction**
   - Implement `cst_to_source.f90`
   - Exact source reconstruction
   - Formatting preservation

2. **Refactoring Support**
   - CST modification with AST validation
   - Preserve user formatting
   - Support rename, extract, inline

3. **Enhanced Diagnostics**
   - Show exact source context
   - Underline error locations
   - Suggest fixes with CST edits

## File Impact Analysis

### Files Requiring Major Changes (30+ files)
All parser modules need CST construction:
- `parser_expressions.f90`
- `parser_declarations.f90`
- `parser_control_flow.f90`
- `parser_statements.f90`
- `parser_dispatcher.f90`
- And 9 more parser files

### Files Requiring Minor Changes (20+ files)
Update to use new AST with UIDs:
- `semantic_analyzer.f90`
- `codegen_core.f90`
- AST visitor implementations

### New Files to Create (15+ files)
- CST infrastructure (5 files)
- CST/AST converters (3 files)
- UID management (2 files)
- Tests (5+ files)

## Testing Strategy

### Level 1: Unit Tests
- CST node creation
- Trivia attachment
- UID generation
- CST/AST conversion

### Level 2: Integration Tests
- Parser → CST → AST flow
- Round-trip source reconstruction
- Semantic equivalence

### Level 3: System Tests
- Full compilation with CST path
- Performance benchmarks
- Memory usage analysis

### Level 4: Regression Tests
- All existing tests must pass
- No functionality regression
- Performance within 10% of current

## Risk Mitigation

### Risk 1: Breaking Existing Functionality
**Mitigation**: Parallel implementation, extensive testing at each phase

### Risk 2: Performance Degradation
**Mitigation**: Arena-based allocation, benchmark at each phase

### Risk 3: Memory Overhead
**Mitigation**: Shared string pools, arena compaction

### Risk 4: Complex Migration
**Mitigation**: Incremental module-by-module migration

## Success Criteria

### Phase Completion Criteria
Each phase must:
- ✅ All existing tests pass
- ✅ New functionality tested
- ✅ No performance regression >10%
- ✅ Documentation updated
- ✅ Code review passed

### Final System Criteria
- ✅ Complete CST/AST separation
- ✅ Exact source reconstruction
- ✅ All trivia preserved
- ✅ Bidirectional CST/AST links
- ✅ External tool API with UIDs
- ✅ No legacy code remaining

---

# GCC 15.2.1 Compatibility Solution (Issue #354)

## Problem Statement
GCC 15.2.1 introduces critical bugs with allocatable components in derived types causing:
- Segmentation faults when accessing `poly_type_t` objects
- Memory corruption during assignment operations  
- Crashes when checking `allocated()` status on undefined objects
- 19+ test failures blocking PR #346 and multiple feature developments

## Root Cause Analysis
The compiler fails to properly initialize allocatable components in derived types, particularly affecting:
1. `poly_type_t` with cached allocatable arrays
2. `substitution_t` with allocatable vars/types arrays
3. `type_env_t` with allocatable names/schemes arrays

## Implementation Strategy

### Immediate Solution: Non-Allocatable Fixed-Size Arrays
Replace allocatable components with fixed-size arrays and explicit size tracking:

```fortran
! Before (crashes with GCC 15.2.1)
type :: substitution_t
    integer :: count = 0
    type(type_var_t), allocatable :: vars(:)
    type(mono_type_t), allocatable :: types(:)
end type

! After (safe with all compilers)
type :: substitution_t
    integer :: count = 0
    integer :: capacity = 0
    integer, parameter :: MAX_SUBST_SIZE = 256
    type(type_var_t) :: vars(MAX_SUBST_SIZE)
    type(mono_type_t) :: types(MAX_SUBST_SIZE)
contains
    procedure :: ensure_capacity => substitution_ensure_capacity
end type
```

### Design Rationale
1. **CORRECTNESS over PERFORMANCE**: Fixed arrays ensure memory safety
2. **Compiler Independence**: Works with all Fortran compilers
3. **Predictable Memory**: No allocation failures at runtime
4. **Arena Migration Path**: Aligns with planned arena architecture

### Implementation Plan

#### Step 1: Type System Refactoring
- Convert `substitution_t` to fixed arrays (MAX_SUBST_SIZE = 256)
- Convert `type_env_t` to fixed arrays (MAX_ENV_SIZE = 1024)  
- Remove all allocatable components from `poly_type_t`
- Add capacity tracking and bounds checking

#### Step 2: Assignment Operator Updates
- Simplify assignment operators to copy fixed arrays
- Remove all `allocated()` checks causing crashes
- Add explicit size/capacity copying
- Ensure deep copy semantics maintained

#### Step 3: Performance Optimization
- Use `count` field for iteration bounds
- Skip unused array elements in operations
- Profile to ensure no performance regression
- Document memory overhead tradeoffs

### Compatibility Testing
- Test with GCC 14.x, 15.1.1, 15.2.1
- Test with Intel Fortran Compiler
- Test with NAG Fortran Compiler
- Ensure no regressions in any compiler

### Long-term Migration
This solution is temporary until arena architecture (Phase 1) provides:
- Stable handle-based type storage
- No allocatable components needed
- Memory managed by arena allocator
- Complete compiler independence

# Implementation Roadmap (FPM-FIRST APPROACH)

## Phase 0: FPM Integration Verification (HIGHEST PRIORITY)
**Goal**: Verify FPM handles all dependency management correctly

### Week 1: FPM Integration Testing
- [ ] Create sample external tool using fortfront as FPM dependency
- [ ] Verify FPM correctly compiles and links all fortfront modules
- [ ] Test that external tools can access fortfront functionality
- [ ] Document simple integration patterns

### Week 2: API Refinement
- [ ] Identify which existing modules should be public API
- [ ] Ensure clean module interfaces for external tool use
- [ ] Add any missing convenience interfaces
- [ ] Document public API patterns

### Week 3: Integration Documentation
- [ ] Create integration examples for different tool types
- [ ] Document FPM dependency patterns
- [ ] Create getting-started guide for external tools
- [ ] Verify cross-platform FPM integration

**Deliverable**: Verified FPM integration with documented patterns for external tool development

## Phase 1: Arena Unification (Issues #368-#376)
**Goal**: Migrate to unified arena architecture with stable handles

### Week 4-5: Foundation
- [ ] #369: Define base arena interface
- [ ] #370: Migrate AST arena to container API
- [ ] #371: Integrate compiler_arena coordinator

### Week 6: Migration
- [ ] #372: Eliminate duplication between arenas
- [ ] #373: Implement type arena container API
- [ ] #374: Add validated handle access

### Week 7: Enhancement
- [ ] #375: Per-node freeing with generations
- [ ] #376: Add performance benchmarks

**Deliverable**: Working system with unified arena, all tests passing

## Phase 2: Stable Identifiers (Issues #377-#378)
**Goal**: Add uid:int64 to all nodes for external tool integration through Fortran modules

### Tasks
- [ ] #377: Add uid field to ast_node base type
- [ ] #378: UID generation and management
- [ ] Expose UID-based node lookup through Fortran modules
- [ ] Update Fortran module interfaces for external tool access
- [ ] Test UID stability across parsing sessions

**Deliverable**: Stable node identifiers accessible through Fortran modules

## Phase 3: CST Implementation (New Issues Needed)
**Goal**: Create concrete syntax tree preserving all source information

### Tasks
- [ ] Define CST node types
- [ ] Modify parser to build CST
- [ ] Create CST→AST converter
- [ ] Add bidirectional CST↔AST links
- [ ] Implement source reconstruction

**Deliverable**: Lossless parsing with exact source reconstruction

## Phase 4: Type System Completion (New Issues Needed)
**Goal**: Full Hindley-Milner type inference with constraints

### Tasks
- [ ] Implement constraint generation
- [ ] Create constraint solver
- [ ] Add occurs check
- [ ] Implement let-polymorphism
- [ ] Add Fortran-specific extensions

**Deliverable**: Complete type inference for all Fortran constructs

## Phase 5: AST Patching (Issue #379)
**Goal**: Transform lazy Fortran to standard Fortran

### Tasks
- [ ] #379: Implement declaration materializer
- [ ] Add program structure normalization
- [ ] Create lazy→standard transformer
- [ ] Integrate into pipeline
- [ ] Add configuration options

**Deliverable**: Lazy Fortran support with x=2 → integer::x

## Phase 6: Plugin Architecture (Issues #380-#382)
**Goal**: External tool integration

### Tasks
- [ ] #380: Create unified API for external tools
- [ ] #381: Comprehensive documentation
- [ ] #382: Complete test suite
- [ ] Create fluff plugin examples
- [ ] Document ffc integration

**Deliverable**: Working plugins for fluff and ffc

## Success Criteria

### Each Phase Must:
1. **Compile**: System builds without errors
2. **Test**: All existing tests pass
3. **Document**: API changes documented
4. **Benchmark**: No performance regression
5. **Integrate**: External tools continue working

### Final System Must:
1. Support lazy Fortran (x=2 without declaration)
2. Provide stable UIDs for external tools
3. Enable exact source reconstruction
4. Deliver 10x+ performance improvement
5. Maintain backward compatibility

---

# Input Validation Module Architecture (Issue #262)

## Executive Summary

The input validation module provides comprehensive validation with enhanced error reporting as a standalone component independent of the frontend transformation pipeline.

## Module Design

### Core Validation Module (`src/input_validation.f90`)
- Comprehensive input validation
- Enhanced error reporting with suggestions
- Standalone module with no circular dependencies
- Support for editor integration and build tools

### Key Features
- **Location Information**: Precise line and column numbers
- **Clear Messages**: Specific problem identification
- **Fix Suggestions**: Helpful recommendations for syntax errors
- **Source Context**: Display of problematic source lines
- **No Silent Failures**: All errors explicitly reported

### Integration Points
- Editor plugins for real-time validation
- Build system integration for pre-compilation checks
- Educational tools for learning Fortran
- CI/CD pipeline validation

## Validation Pipeline

```
Source Code
    ↓
[Tokenizer] → Token validation
    ↓
[Syntax Check] → Structure validation
    ↓
[Semantic Check] → Type and scope validation
    ↓
Validation Results (errors, warnings, suggestions)
```

## Error Reporting Format

```fortran
type :: validation_error_t
    integer :: line, column
    character(len=:), allocatable :: message
    character(len=:), allocatable :: suggestion
    integer :: severity  ! ERROR, WARNING, INFO
    character(len=:), allocatable :: source_context
end type
```

## Current Status
✅ **Implemented**: Complete validation module with comprehensive error reporting
✅ **Tested**: Extensive test coverage for various error scenarios
✅ **Integrated**: Used throughout compilation pipeline