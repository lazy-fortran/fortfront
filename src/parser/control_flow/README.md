# Parser Control Flow

## Purpose

This directory handles parsing of control flow constructs: if/elseif/else blocks, select case/type statements, where/elsewhere array conditionals, do loops, and forall constructs. Control flow parsing requires careful tracking of nesting depth and proper pairing of begin/end statements.

## File Index

| File | Description |
|------|-------------|
| parser_control_flow.f90 | Public facade for control flow parsing |
| parser_control_flow_router.f90 | Route to appropriate control flow parser based on keyword |
| parser_control_statements.f90 | Control transfer statements: exit, cycle, return |
| parser_if_constructs.f90 | Block if parsing: if/elseif/else/endif |
| parser_if_statements.f90 | Statement if parsing: `if (cond) statement` |
| parser_select_constructs.f90 | Select case/type/rank facade (includes four parts below) |
| parser_select_constructs_helpers.inc | Shared header, layout, end-select, and selector helpers |
| parser_select_constructs_case.inc | SELECT CASE arm and construct parsing |
| parser_select_constructs_type.inc | SELECT TYPE guard and construct parsing |
| parser_select_constructs_rank.inc | SELECT RANK arm and construct parsing |
| parser_do_constructs.f90 | Do loop facade (includes two parts below) |
| parser_do_constructs_part1.inc | Do loop parsing part 1: counted loops, do while |
| parser_do_constructs_part2.inc | Do loop parsing part 2: loop labeling, nesting |
| parser_forall.f90 | Forall construct parsing (array-parallel loops) |

## Key Concepts

**If Constructs**
- **Block if**: Multi-statement conditional
  ```fortran
  if (condition) then
      statements
  elseif (condition2) then
      statements
  else
      statements
  end if
  ```
- **Statement if**: Single-statement conditional
  ```fortran
  if (condition) statement
  ```
- **Arithmetic if** (legacy): `if (expr) label1, label2, label3`

**Select Constructs**
- **Select case**: Multi-way branch based on value
  ```fortran
  select case (variable)
  case (1)
      statements
  case (2:5)
      statements
  case default
      statements
  end select
  ```
- **Select type**: Polymorphic type selection (Fortran 2003+)
  ```fortran
  select type (var)
  type is (integer)
      statements
  type is (real)
      statements
  class default
      statements
  end select
  ```

**Do Loops**
- **Counted loop**: `do i = 1, 10, 2` (start, end, stride)
- **While loop**: `do while (condition)`
- **Infinite loop**: `do` (exit via `exit` statement)
- **Named loops**: `outer: do ... end do outer`
- **Legacy labeled loops**: `do 100 i = 1, 10` (use label instead of `end do`)

**Where Constructs**
- Array-parallel conditional assignment
- Operate on entire arrays element-wise
- **Simple where**: `where (mask) array = expression`
- **Block where**:
  ```fortran
  where (mask)
      array1 = expression1
      array2 = expression2
  elsewhere (mask2)
      array1 = expression3
  elsewhere
      array1 = expression4
  end where
  ```

**Forall Constructs**
- Explicit array-parallel loops (Fortran 95+)
- Specify iteration indices and mask
- All iterations independent (can execute in any order)
- **Single statement**: `forall (i=1:n, j=1:m, mask) array(i,j) = expression`
- **Block forall**:
  ```fortran
  forall (i=1:n, j=1:m, mask)
      array(i,j) = expression
  end forall
  ```

**Nesting and Pairing**
- Track nesting depth for proper pairing
- Match `if` with `end if`, `do` with `end do`, etc.
- Named constructs help with deeply nested structures
- Error recovery on mismatched pairs

**Exit and Cycle**
- **Exit**: Terminate loop immediately
  - `exit` - exit innermost loop
  - `exit loop_name` - exit named loop
- **Cycle**: Skip to next iteration
  - `cycle` - cycle innermost loop
  - `cycle loop_name` - cycle named loop

## Dependencies

**Parser Core**
- `parser/core/parser_state` - State management, nesting tracking
- `parser/core/parser_dispatcher` - Control flow routing

**Parser Expressions**
- `parser/expressions/parser_expressions` - Condition expression parsing

**Parser Statements**
- `parser/statements/` - Statement body parsing

**AST Factory**
- `ast/factory/ast_factory_control` - Control flow node creation

**Common Utilities**
- `common/identifier_table` - Loop name management
