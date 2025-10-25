# Monomorphization AST Transformation Implementation Plan

## Overview
Transform AST after semantic analysis to create monomorphized function variants.

## Input
- Typed AST with function_def_node for generic function
- signatures_map_t with all unique call site signatures

## Output (AST Structure)
```
module_node "auto_<function_name>"
  declaration_indices:
    - interface_block_node "<function_name>"
        procedure_indices:
          - module_procedure_node [variant names...]
  procedure_indices: (after contains)
    - function_def_node "<function_name>__k2_k2" (cloned with types)
    - function_def_node "<function_name>__k3_k3" (cloned with types)

program_node "main"
  use_statement_node "auto_<function_name>"
  ... rest of program
```

## Implementation Steps

### 1. Find Functions Needing Monomorphization
- Traverse AST to find function_def_node
- Check signatures_map for multiple signatures
- Collect list of functions to transform

### 2. For Each Function
a. **Clone function_def_node** for each signature
   - Use arena allocation
   - Update parameter types based on signature
   - Update return type based on signature  
   - Update function name to mangled name
   - Clone body (expressions stay same, just types change)

b. **Create module_procedure_node**
   - Allocate procedure_names array
   - Fill with mangled names

c. **Create interface_block_node**
   - Set name = original function name
   - Set procedure_indices to point to module_procedure_node

d. **Create module_node**  
   - Set name = "auto_" // function_name
   - Add interface_block to declaration_indices
   - Add cloned functions to procedure_indices
   - Set has_contains = true

### 3. Update Program Node
- Add use_statement_node for generated module
- Remove external declaration for monomorphized functions

### 4. Update Root Index
- If wrapping in module, may need to create multi-unit structure

## Arena Considerations
- All nodes must be allocated in arena
- Get new indices from arena
- Update parent/child relationships via indices
- Be careful with allocatable arrays

## Key Functions Needed
```fortran
! Clone function with new types
function clone_function_with_types(arena, func_idx, signature, mangled_name) &
    result(new_idx)

! Create interface block  
function create_interface_block(arena, name, procedure_indices) result(idx)

! Create module wrapping monomorphized functions
function create_monomorphization_module(arena, orig_name, func_indices, &
    interface_idx) result(mod_idx)

! Update program to use module
subroutine add_use_statement(arena, prog_idx, module_name)
```

## Testing Strategy
1. Unit test: clone single function
2. Unit test: create interface block
3. Unit test: create module
4. Integration: full monomorphization
5. Verify codegen produces correct Fortran
6. Verify gfortran compiles and runs correctly

## Success Criteria
- AST nodes properly created
- Any codegen can generate correct Fortran
- gfortran compiles output
- Program runs and produces correct results
