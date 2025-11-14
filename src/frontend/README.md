# Frontend

## Purpose

The frontend orchestrates the complete transformation pipeline from source text to standardized Fortran output. It manages the high-level flow through lexing, parsing, semantic analysis, and code generation, while handling special cases like mixed constructs (`.lf` files with embedded standard Fortran), program structure detection, and statement boundary identification.

The frontend provides the primary API entry point for transforming lazy Fortran to standard Fortran and for round-trip validation of standard Fortran.

## File Index

| File | Description |
|------|-------------|
| frontend_tooling_api.f90 | Public API for tool integration (linters, language servers) |
| frontend_transformation_pipeline.f90 | Main transformation orchestration: lex → parse → semantic → codegen |
| frontend_transformation_structure.f90 | Program structure transformation (wrap bare statements in program) |
| frontend_transformation_analysis.f90 | Semantic analysis integration and coordination |
| frontend_transformation_semantics.f90 | Semantic phase coordination |
| frontend_transformation_common.f90 | Shared transformation utilities |
| frontend_mixed_constructs.f90 | Handle `.lf` files with embedded standard Fortran blocks |
| frontend_program_structure.f90 | Detect program structure (module vs program vs bare statements) |
| frontend_program_units.f90 | Program unit identification and extraction |
| frontend_program_unit_scanner.f90 | Scan for program unit boundaries |
| frontend_statement_boundary.f90 | Statement boundary detection across program units |
| frontend_statement_processing.f90 | Statement-level processing and normalization |
| frontend_token_normalization.f90 | Token stream normalization before parsing |

## Key Concepts

**Transformation Pipeline**
1. **Lexing**: Tokenize source text
2. **Parsing**: Build AST from tokens
3. **Semantic Analysis**: Type inference, scope resolution
4. **Code Generation**: Emit standardized Fortran

**Mixed Construct Handling**
- `.lf` files may contain both lazy and standard Fortran
- Standard blocks wrapped in special markers: `!fortfront:standard_begin` / `!fortfront:standard_end`
- Standard blocks passed through unchanged
- Lazy blocks transformed via full pipeline
- See `docs/MIXED_CONSTRUCTS_GUIDE.md`

**Program Structure Detection**
- **Bare statements**: Wrap in `program main ... end program`
- **Single procedure**: Keep as standalone procedure
- **Module**: Preserve module structure
- **Complete program**: Use existing structure

**Statement Boundary Detection**
- Identify statement boundaries in free-form source
- Handle continuation lines (ampersand)
- Detect statement keywords in various contexts
- Support legacy fixed-form (optional)

**Token Normalization**
- Normalize keyword case
- Handle operator variants (`==` vs `.eq.`)
- Collapse whitespace patterns
- Prepare clean token stream for parser

**API Interfaces**
- **CLI**: `fortfront input.lf > output.f90`
- **Library**: `transform_lazy_fortran_string(input, output, errors)`
- **Tooling**: `parse_fortran_for_tools(source, ast, context)`

## Dependencies

**Lexer**
- `lexer/` - Tokenization of source text

**Parser**
- `parser/` - AST construction from tokens

**Semantic Analysis**
- `semantic/` - Type inference and validation
- `semantic/analyzers/` - Semantic analyzers

**Code Generation**
- `codegen/` - Emit standardized Fortran

**Analysis**
- `analysis/call_graph` - Procedure analysis for type inference

**AST**
- `ast/` - AST data structures and traversal

**Utilities**
- `utilities/` - String utilities, debug tracing
