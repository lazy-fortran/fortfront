# Parser Procedures

## Purpose

This directory handles parsing of procedure definitions (functions and subroutines): signatures, parameter lists, result types, procedure bodies, and procedure calls. Procedures are the fundamental unit of executable code organization in Fortran.

## File Index

| File | Description |
|------|-------------|
| parser_procedure_definitions.f90 | Main procedure definition parsing orchestration |
| parser_procedure_definition_bodies.f90 | Procedure body parsing (declarations and executable statements) |
| parser_block_statement_utils.f90 | Block statement boundary detection for IF/DO/SELECT constructs |
| parser_procedure_signatures.f90 | Procedure signature parsing (parameters, result type) |
| parser_prefix_buffer.f90 | Parse prefix attributes (pure, elemental, recursive) |
| parser_result_types.f90 | Function result type specification |
| parser_call.f90 | Call statement and function call parsing |
| parser_procedure_shared.f90 | Shared procedure parsing utilities |

## Key Concepts

**Function vs Subroutine**
- **Function**: Returns a value, used in expressions: `y = sqrt(x)`
  - Syntax: `function name(args) result(res)` or `type function name(args)`
  - Result variable declared implicitly or explicitly
- **Subroutine**: No return value, invoked via call statement: `call sort(array)`
  - Syntax: `subroutine name(args)`
  - Parameters modified via `intent(out)` or `intent(inout)`

**Procedure Signature**
- **Parameters**: Dummy argument list
  - Named parameters: `function add(a, b)`
  - Type specifications: `integer, intent(in) :: a, b`
  - Optional parameters: `integer, optional :: c`
  - Keyword parameters: `call sub(x=1, y=2)`
- **Result type**: Function return type
  - Implicit: `function add(a, b)` (name is result variable)
  - Explicit: `function add(a, b) result(sum)` (separate result variable)
  - Type prefix: `integer function add(a, b)`

**Prefix Attributes**
- **pure**: No side effects, can be called in parallel
- **elemental**: Operates element-wise on arrays
- **recursive**: Can call itself
- **impure**: Opposite of pure (Fortran 2008+)
- Combined: `pure recursive function factorial(n)`

**Procedure Body Structure**
1. **Specification section**: Variable declarations, use statements, implicit none
2. **Execution section**: Executable statements
3. **Contains section**: Internal procedures (optional)
4. **End statement**: `end function name` or `end subroutine name`

**Result Type Specification**
- **Implicit naming**: Result variable has same name as function
  ```fortran
  function add(a, b)
      add = a + b  ! add is the result variable
  end function
  ```
- **Explicit result**: Separate result variable name
  ```fortran
  function add(a, b) result(sum)
      sum = a + b  ! sum is the result variable
  end function
  ```
- **Type prefix**: Specify type before function keyword
  ```fortran
  integer function add(a, b)
      add = a + b
  end function
  ```

**Parameter Intent**
- **intent(in)**: Read-only, not modified
- **intent(out)**: Write-only, must be set before return
- **intent(inout)**: Read-write, modified and returned
- **No intent**: Fortran 77 style, avoid in modern code

**Internal Procedures**
- Declared after `contains` in parent procedure
- Access parent's variables (host association)
- Private to parent (not externally visible)
- Enable code organization and reuse

**Procedure Calls**
- **Subroutine call**: `call subroutine_name(args)`
- **Function call**: Used in expressions: `y = function_name(args)`
- **Positional arguments**: Order matters: `call sub(1, 2, 3)`
- **Keyword arguments**: Order independent: `call sub(z=3, x=1, y=2)`

## Dependencies

**Parser Core**
- `parser/core/parser_state` - State management
- `parser/core/parser_dispatcher` - Statement routing

**Parser Declarations**
- `parser/declarations/` - Parameter declarations, type specs

**Parser Statements**
- `parser/statements/` - Executable statement parsing

**AST Factory**
- `ast/factory/ast_factory_procedures` - Procedure node creation

**Common Utilities**
- `common/identifier_table` - Identifier management
- `common/declaration_attribute_utils` - Attribute handling
