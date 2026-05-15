# Common

## Purpose

This directory provides shared utilities used throughout the fortfront compiler infrastructure. These utilities handle identifier management, unique ID generation, and declaration attribute processing. They provide foundational services that all compiler phases depend on.

## File Index

| File | Description |
|------|-------------|
| identifier_table.f90 | String interning for identifiers, case-insensitive lookup, memory-efficient storage |
| uid_generator.f90 | Unique ID generation for AST nodes, deterministic ID assignment with OpenMP-guarded global state |
| declaration_attribute_utils.f90 | Parse and manipulate Fortran declaration attributes (intent, allocatable, etc.) |

## Key Concepts

**Identifier Interning**
- Store each unique identifier string once
- Return integer ID for lookups
- Case-insensitive comparison (Fortran semantics)
- Memory-efficient: O(1) comparison via IDs
- Thread-safe concurrent access

**Identifier Table Operations**
```fortran
! Register identifier, get ID
id = identifier_table_add("variable_name")

! Lookup identifier string by ID
str = identifier_table_get(id)

! Case-insensitive lookup
id1 = identifier_table_add("MyVar")
id2 = identifier_table_add("myvar")
! id1 == id2 (Fortran is case-insensitive)
```

**Unique ID Generation**
- Monotonically increasing IDs for AST nodes
- Deterministic assignment (same input → same IDs)
- Used for node identity and comparison
- Separate ID spaces for different entity types
- The compatibility generator uses module state guarded by OpenMP critical
  regions when FortFront is built with OpenMP.

**Declaration Attributes**
- Parse attribute strings: `intent(in)`, `allocatable`, `pointer`, etc.
- Validate attribute compatibility
- Emit attribute strings for codegen
- Handle Fortran 2003/2008/2018 attributes

**Attribute Validation**
- Detect incompatible attribute combinations
- Enforce Fortran standard rules
- Provide clear error messages
- Support incremental attribute building

## Dependencies

**Standard Library**
- `stdlib` - String utilities, hash tables

**Utilities**
- `utilities/string_utils` - String manipulation
