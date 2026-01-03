# Parser Declarations

## Purpose

This directory handles parsing of all Fortran declaration constructs: type specifications, variable declarations, derived types, interface blocks, module structures, and parameter statements. Declaration parsing is complex due to Fortran's flexible syntax and context-sensitive keywords.

## File Index

| File | Description |
|------|-------------|
| parser_declarations.f90 | Public facade for declaration parsing |
| parser_declarations_core_module.f90 | Core declaration parsing logic |
| parser_declarations_construction_module.f90 | Declaration AST node construction |
| parser_declarations_multi_module.f90 | Multi-variable declaration parsing (e.g., `integer :: a, b, c`) |
| parser_declarations_type_spec_module.f90 | Type specification parsing (e.g., `integer(kind=4)`) |
| parser_declarations_type_spec_support_module.f90 | Type spec support utilities |
| parser_declarations_derived_module.f90 | Derived type declaration parsing |
| parser_declaration_attributes_module.f90 | Attribute parsing: `intent`, `allocatable`, `pointer`, etc. |
| parser_type_specifications.f90 | Type spec orchestration |
| parser_type_spec_attributes_mod.f90 | Type spec attribute handling |
| parser_type_spec_result_mod.f90 | Function result type specification |
| parser_type_spec_tokens_mod.f90 | Type spec token utilities |
| parser_type_definitions.f90 | Type definition statements |
| parser_type_hooks.f90 | Type definition extension points |
| parser_dimension_statements_module.f90 | Dimension statement parsing |
| parser_parameter_handling.f90 | Parameter statement parsing (`parameter :: pi = 3.14`) |
| parser_interface_blocks.f90 | Interface block parsing |
| parser_interface_block_headers_module.f90 | Interface block headers |
| parser_interface_prefix_module.f90 | Interface prefix attributes (`pure`, `elemental`) |
| parser_interface_import_module.f90 | Interface import statements |
| parser_interface_module_procedures_module.f90 | Module procedure declarations in interfaces |
| parser_module_structures.f90 | Module declaration parsing |
| parser_template_blocks.f90 | LFortran generics parsing: template blocks |
| parser_instantiate_statement.f90 | LFortran generics parsing: instantiate statements |
| parser_block_data.f90 | Block data construct parsing (legacy) |
| parser_definition_statements.f90 | Definition statement parsing |

## Key Concepts

**Type Specifications**
- **Intrinsic types**: `integer`, `real`, `complex`, `character`, `logical`
- **Kind parameters**: `integer(4)`, `real(kind=8)`, `real(dp)`
- **Character length**: `character(len=10)`, `character(len=*)`, `character(*)`
- **Derived types**: `type(my_type)`

**Declaration Attributes**
- **Intent**: `intent(in)`, `intent(out)`, `intent(inout)`
- **Allocation**: `allocatable`, `pointer`, `target`
- **Dimension**: `dimension(:)`, `dimension(10, 20)`
- **Access**: `public`, `private`
- **Procedure attributes**: `pure`, `elemental`, `recursive`
- **Legacy**: `save`, `parameter`, `external`, `intrinsic`

**Multi-Variable Declarations**
- Parse: `integer :: a, b, c`
- Create separate AST nodes for each variable
- Share common attributes across variables
- Handle individual initializers: `integer :: a = 5, b = 10`

**Derived Type Declarations**
- Parse type definition: `type :: my_type ... end type`
- Component declarations within type
- Type-bound procedures
- Type parameters (Fortran 2003+)
- Type extension: `type, extends(parent_type) :: child_type`

**Interface Blocks**
- Parse interface: `interface ... end interface`
- Generic interfaces: `interface operator(+)`
- Specific interfaces: `interface subroutine_name`
- Module procedure declarations
- Abstract interfaces

**Module Structures**
- Module header: `module module_name`
- Module body: declarations, procedures
- Module procedures: `contains` section
- Submodules (Fortran 2008+)

**Context-Sensitive Parsing**
- `data` as keyword vs identifier: `data x /5/` vs `data = 5`
- `dimension` as keyword vs identifier: `dimension(10)` vs `dimension = 10`
- `parameter` as keyword vs identifier: `parameter :: x = 5` vs `parameter = 5`

## Dependencies

**Parser Core**
- `parser/core/parser_state` - State management
- `parser/core/parser_utilities` - Parsing utilities

**AST Factory**
- `ast/factory/ast_factory_declarations` - Declaration node creation

**Common Utilities**
- `common/declaration_attribute_utils` - Attribute processing
- `common/identifier_table` - Identifier management
