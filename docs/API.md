# FortFront Library API Reference

This document describes the public library API for integrating FortFront into downstream tools such as linters, compilers, formatters, and other analysis tools.

## Overview

FortFront provides a modular API organized into seven core modules:

1. **lexer_api** - Tokenization and lexical analysis
2. **parser_api** - Token parsing and AST construction
3. **ast_api** - AST node types and traversal utilities
4. **semantic_api** - Type inference and semantic validation
5. **codegen_api** - Standard Fortran code generation
6. **error_api** - Error handling and reporting
7. **transformation_api** - High-level transformation pipeline
8. **frontend_tooling_api** - Convenience functions for tooling integration

Each module exposes only the types, constants, and procedures intended for library consumers, hiding internal implementation details.

## Module: lexer_api

Provides tokenization functionality for Fortran source code.

### Types

#### token_t
Represents a single token with position information.

```fortran
type :: token_t
    integer :: kind
    character(len=:), allocatable :: lexeme
    integer :: line
    integer :: column
end type token_t
```

#### tokenize_result_t
Result type containing tokens and any lexer errors.

#### lexer_options_t
Configuration options for the lexer.

### Token Kind Constants

- `TK_EOF` - End of file
- `TK_IDENTIFIER` - Variable/function/type names
- `TK_NUMBER` - Numeric literals
- `TK_STRING` - String literals
- `TK_OPERATOR` - Operators (+, -, *, /, etc.)
- `TK_KEYWORD` - Fortran keywords (program, function, if, etc.)
- `TK_NEWLINE` - Line breaks
- `TK_COMMENT` - Comments
- `TK_WHITESPACE` - Whitespace
- `TK_UNKNOWN` - Unrecognized tokens

### Procedures

#### tokenize_core
```fortran
subroutine tokenize_core(source, tokens)
    character(len=*), intent(in) :: source
    type(token_t), allocatable, intent(out) :: tokens(:)
```
Tokenize source code, allocating error on failure.

#### tokenize_safe
```fortran
function tokenize_safe(source) result(tokens)
    character(len=*), intent(in) :: source
    type(token_t), allocatable :: tokens(:)
```
Safe tokenization that never allocates on error.

#### token_type_name
```fortran
function token_type_name(kind) result(name)
    integer, intent(in) :: kind
    character(len=:), allocatable :: name
```
Get human-readable name for token kind.

## Module: parser_api

Provides parsing functionality to convert tokens into AST.

### Types

#### parse_result_with_index_t
Result type containing AST root index and parse status.

#### ast_arena_t
Arena allocator for AST nodes (from ast_api).

#### compiler_arena_t
Compiler arena managing both AST and symbol tables.

### Arena Management

#### create_compiler_arena
```fortran
function create_compiler_arena() result(arena)
    type(compiler_arena_t) :: arena
```

#### destroy_compiler_arena
```fortran
subroutine destroy_compiler_arena(arena)
    type(compiler_arena_t), intent(inout) :: arena
```

### Parsing Procedures

#### parse_tokens
```fortran
subroutine parse_tokens(tokens, arena, root_index, error_msg)
    type(token_t), intent(in) :: tokens(:)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(out) :: root_index
    character(len=512), intent(out) :: error_msg
```
Parse tokens into AST stored in arena.

#### parse_tokens_safe
Safe version with structured error handling.

### Program Structure Analysis

- `find_program_unit_boundary` - Locate program unit boundaries
- `is_function_start` - Check if tokens start a function
- `is_end_function` - Check for function end
- `parse_program_unit` - Parse single program unit

### Control Flow Detection

- `is_do_loop_start` - Check for DO loop start
- `is_do_while_start` - Check for DO WHILE start
- `is_select_case_start` - Check for SELECT CASE start
- `is_end_do` - Check for END DO
- `is_end_select` - Check for END SELECT
- `is_if_then_start` - Check for IF...THEN start
- `is_end_if` - Check for END IF

## Module: ast_api

Provides access to AST node types and arena operations.

### Base Types

#### ast_node
Base class for all AST nodes (polymorphic).

#### ast_node_wrapper
Wrapper for passing nodes through interfaces.

#### ast_visitor_base_t
Base type for implementing visitor pattern.

#### string_t
String type used in AST.

### Literal Type Constants

- `LITERAL_INTEGER`
- `LITERAL_REAL`
- `LITERAL_STRING`
- `LITERAL_LOGICAL`
- `LITERAL_ARRAY`
- `LITERAL_COMPLEX`

### Core Node Types

- `program_node` - Program unit
- `assignment_node` - Assignment statement
- `pointer_assignment_node` - Pointer assignment (=>)
- `identifier_node` - Variable/function reference
- `literal_node` - Literal values
- `binary_op_node` - Binary operations (+, -, *, /, etc.)
- `unary_op_node` - Unary operations (-, .NOT., etc.)
- `call_or_subscript_node` - Function calls or array subscripts
- `array_literal_node` - Array constructors
- `component_access_node` - Derived type component access
- `range_subscript_node` - Array section subscripts

### Procedure Node Types

- `function_def_node` - Function definition
- `subroutine_def_node` - Subroutine definition
- `function_call_node` - Function call expression
- `subroutine_call_node` - Subroutine call statement

### Control Flow Node Types

- `if_node` - IF statement/construct
- `do_loop_node` - DO loop
- `do_while_loop_node` - DO WHILE loop
- `select_case_node` - SELECT CASE construct
- `case_node` - CASE clause
- `exit_node` - EXIT statement
- `cycle_node` - CYCLE statement

### Data Structure Node Types

- `module_node` - MODULE definition
- `interface_node` - INTERFACE block
- `type_def_node` - Derived type definition
- `variable_decl_node` - Variable declaration

### Visitor Support

#### ast_visitor_t
Complete visitor implementation with default visit methods.

### Traversal Utilities

#### traverse_ast
```fortran
subroutine traverse_ast(arena, node_index, visitor)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: node_index
    class(ast_visitor_base_t), intent(inout) :: visitor
```
Traverse AST using visitor pattern.

#### count_nodes
```fortran
function count_nodes(arena, node_index) result(count)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: node_index
    integer :: count
```
Count total nodes in AST subtree.

#### find_nodes_by_type
```fortran
function find_nodes_by_type(arena, node_index, node_type) result(indices)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: node_index
    character(len=*), intent(in) :: node_type
    integer, allocatable :: indices(:)
```
Find all nodes matching a specific type name.

## Module: semantic_api

Provides type inference and semantic validation.

### Types

#### semantic_context_t
Main context holding symbol tables and type environment.

#### mono_type_t
Monomorphic type (concrete type like integer, real).

#### poly_type_t
Polymorphic type (generic type with type variables).

#### type_env_t
Type environment mapping identifiers to types.

#### type_var_t
Type variable for inference.

#### substitution_t
Type substitution mapping variables to concrete types.

### Context Management

#### create_semantic_context
```fortran
function create_semantic_context() result(ctx)
    type(semantic_context_t) :: ctx
```

#### analyze_program
```fortran
subroutine analyze_program(arena, root_index, ctx)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: root_index
    type(semantic_context_t), intent(inout) :: ctx
```

#### has_semantic_errors
```fortran
function has_semantic_errors(ctx) result(has_errors)
    type(semantic_context_t), intent(in) :: ctx
    logical :: has_errors
```

### Type Constructors

#### create_mono_type
```fortran
function create_mono_type(kind) result(mono)
    integer, intent(in) :: kind
    type(mono_type_t) :: mono
```

#### create_poly_type
```fortran
function create_poly_type(vars, mono) result(poly)
    type(type_var_t), intent(in) :: vars(:)
    type(mono_type_t), intent(in) :: mono
    type(poly_type_t) :: poly
```

#### create_type_var
```fortran
function create_type_var(id) result(var)
    integer, intent(in) :: id
    type(type_var_t) :: var
```

### Type Constants

- `TVAR` - Type variable
- `TINT` - Integer type
- `TREAL` - Real type
- `TCHAR` - Character type
- `TLOGICAL` - Logical type
- `TFUN` - Function type
- `TARRAY` - Array type
- `TCOMPLEX` - Complex type
- `TDOUBLE` - Double precision type
- `TDERIVED` - Derived type

### Scope Management

#### scope_stack_t
Stack of lexical scopes.

#### create_scope_stack
```fortran
function create_scope_stack() result(stack)
    type(scope_stack_t) :: stack
```

#### push_scope / pop_scope
```fortran
subroutine push_scope(stack)
    type(scope_stack_t), intent(inout) :: stack

subroutine pop_scope(stack)
    type(scope_stack_t), intent(inout) :: stack
```

### Error Handling

#### error_collection_t
Collection of errors found during analysis.

#### result_t
Generic result type for operations that may fail.

## Module: codegen_api

Provides Standard Fortran code generation from AST.

### Code Generation Procedures

#### generate_code_from_arena
```fortran
function generate_code_from_arena(arena) result(code)
    type(ast_arena_t), intent(in) :: arena
    character(len=:), allocatable :: code
```
Generate Fortran code from entire arena.

#### generate_code_polymorphic
```fortran
function generate_code_polymorphic(node) result(code)
    class(ast_node), intent(in) :: node
    character(len=:), allocatable :: code
```
Generate code from a single polymorphic node.

#### initialize_codegen
```fortran
subroutine initialize_codegen()
```
Initialize code generator state (call once at startup).

### Configuration Procedures

#### set_type_standardization / get_type_standardization
```fortran
subroutine set_type_standardization(enabled)
    logical, intent(in) :: enabled

function get_type_standardization() result(enabled)
    logical :: enabled
```
Control whether types are standardized (e.g., INTEGER*4 to INTEGER).

#### set_indent_config / get_indent_config
```fortran
subroutine set_indent_config(spaces_per_level)
    integer, intent(in) :: spaces_per_level

function get_indent_config() result(spaces_per_level)
    integer :: spaces_per_level
```
Configure indentation.

#### set_line_length_config / get_line_length_config
```fortran
subroutine set_line_length_config(max_length)
    integer, intent(in) :: max_length

function get_line_length_config() result(max_length)
    integer :: max_length
```
Configure maximum line length (for continuations).

### Utility Procedures

#### add_line_continuations
```fortran
function add_line_continuations(code, max_len) result(wrapped)
    character(len=*), intent(in) :: code
    integer, intent(in) :: max_len
    character(len=:), allocatable :: wrapped
```
Add line continuations to code exceeding max length.

## Module: error_api

Provides error handling and reporting.

### Types

#### error_record_t
Single error record with location and message.

```fortran
type :: error_record_t
    integer :: severity
    integer :: line
    integer :: column
    character(len=:), allocatable :: message
    character(len=:), allocatable :: source_context
end type error_record_t
```

#### error_collection_t
Collection of error records.

#### error_context_t
Context information for formatting error messages.

### Procedures

#### create_error_context
```fortran
function create_error_context(line, column, source) result(ctx)
    integer, intent(in) :: line, column
    character(len=*), intent(in) :: source
    type(error_context_t) :: ctx
```

#### create_error_context_from_token
```fortran
function create_error_context_from_token(token, source) result(ctx)
    type(token_t), intent(in) :: token
    character(len=*), intent(in) :: source
    type(error_context_t) :: ctx
```

#### format_error_message
```fortran
function format_error_message(record, ctx) result(formatted)
    type(error_record_t), intent(in) :: record
    type(error_context_t), intent(in) :: ctx
    character(len=:), allocatable :: formatted
```

### Error Severity Constants

- `ERROR_INFO` - Informational message
- `ERROR_WARNING` - Warning (non-fatal)
- `ERROR_ERROR` - Error (recoverable)
- `ERROR_FATAL` - Fatal error (unrecoverable)

## Module: transformation_api

High-level transformation from Lazy Fortran to Standard Fortran.

### Types

#### format_options_t
Formatting options for output code.

#### transform_context_t
Context tracking transformation state.

### Procedures

#### transform_lazy_fortran_string
```fortran
subroutine transform_lazy_fortran_string(input, output)
    character(len=*), intent(in) :: input
    character(len=:), allocatable, intent(out) :: output
```
Transform Lazy Fortran string to Standard Fortran (simplest API).

#### transform_lazy_fortran_string_with_format
```fortran
subroutine transform_lazy_fortran_string_with_format(input, output, options)
    character(len=*), intent(in) :: input
    character(len=:), allocatable, intent(out) :: output
    type(format_options_t), intent(in) :: options
```
Transform with custom formatting options.

#### transform_with_context
```fortran
subroutine transform_with_context(input, output, ctx)
    character(len=*), intent(in) :: input
    character(len=:), allocatable, intent(out) :: output
    type(transform_context_t), intent(inout) :: ctx
```
Transform with full context access for error inspection.

### Input Mode Constants

- `INPUT_MODE_LAZY` - Input is Lazy Fortran
- `INPUT_MODE_STANDARD` - Input is Standard Fortran

### Utility Procedures

#### detect_input_mode_from_content
```fortran
function detect_input_mode_from_content(source) result(mode)
    character(len=*), intent(in) :: source
    integer :: mode
```
Auto-detect whether source is Lazy or Standard Fortran.

## Module: frontend_tooling_api

Convenience functions for tool developers.

### Types

#### tooling_parse_options_t
```fortran
type :: tooling_parse_options_t
    logical :: run_semantics = .false.
    logical :: reuse_arena = .false.
end type tooling_parse_options_t
```
Options for tooling parse operations.

### Procedures

#### tooling_load_ast_from_string
```fortran
subroutine tooling_load_ast_from_string(source_code, arena, root_index, &
                                        error_msg, options, tokens)
    character(len=*), intent(in) :: source_code
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(out) :: root_index
    character(len=:), allocatable, intent(out) :: error_msg
    type(tooling_parse_options_t), intent(in), optional :: options
    type(token_t), allocatable, intent(out), optional :: tokens(:)
```
Load AST from string with optional semantic analysis.

#### tooling_load_ast_from_file
```fortran
subroutine tooling_load_ast_from_file(path, arena, root_index, error_msg, &
                                      options, tokens)
    character(len=*), intent(in) :: path
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(out) :: root_index
    character(len=:), allocatable, intent(out) :: error_msg
    type(tooling_parse_options_t), intent(in), optional :: options
    type(token_t), allocatable, intent(out), optional :: tokens(:)
```
Load AST from file with optional semantic analysis.

## Usage Examples

### Simple Transformation

```fortran
use transformation_api, only: transform_lazy_fortran_string

character(len=:), allocatable :: input, output

input = "x = 5"
call transform_lazy_fortran_string(input, output)
print *, trim(output)
```

### Lexing Only

```fortran
use lexer_api, only: tokenize_core, token_t, token_type_name

character(len=:), allocatable :: source
type(token_t), allocatable :: tokens(:)
integer :: i

source = "x = 5"
call tokenize_core(source, tokens)

do i = 1, size(tokens)
    print *, token_type_name(tokens(i)%kind), ': ', tokens(i)%lexeme
end do
```

### Full Pipeline

```fortran
use lexer_api
use parser_api
use ast_api
use codegen_api

character(len=*), parameter :: source = "x = 5"
type(token_t), allocatable :: tokens(:)
type(ast_arena_t) :: arena
integer :: root_index
character(len=512) :: error_msg
character(len=:), allocatable :: code

call tokenize_core(source, tokens)
arena = create_ast_arena()
call parse_tokens(tokens, arena, root_index, error_msg)
code = generate_code_from_arena(arena)
print *, trim(code)
```

## Error Handling Pattern

All APIs follow consistent error handling:

1. Allocatable `error_msg` parameters (empty string = success)
2. Result types with status fields
3. Error collections for batch operations

Example:

```fortran
use frontend_tooling_api

type(ast_arena_t) :: arena
integer :: root_index
character(len=:), allocatable :: error_msg

call tooling_load_ast_from_string("x = 5", arena, root_index, error_msg)

if (len_trim(error_msg) > 0) then
    print *, "Error: ", trim(error_msg)
else
    print *, "Success! Root node index: ", root_index
end if
```

## Threading and Safety

- All APIs are reentrant when using separate context objects
- Arena allocators are NOT thread-safe; use one arena per thread
- Error collections are local to each context
- Global codegen configuration (indent, line length) should be set once at startup

## Performance Tips

1. Reuse arenas with `reuse_arena=.true.` for repeated parsing
2. Use `tokenize_safe` for hot paths (avoids allocation on error)
3. Traverse AST once; cache results if needed multiple times
4. For large files, consider streaming approaches with `parse_program_unit`

## Integration Checklist

When integrating FortFront as a library:

- Link against fpm-built library: `fpm build --flag "-fPIC"`
- Import only the API modules needed for your use case
- Initialize codegen configuration once at startup
- Use tooling APIs for simplest integration
- Implement custom visitors for AST analysis
- Handle errors consistently with allocatable strings
- Consider arena reuse for batch processing

## See Also

- LIBRARY_USAGE.md - Complete working examples
- README.md - CLI usage and quick start
- examples/ - Sample programs using the API
