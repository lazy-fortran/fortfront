# AST Arena Migration Guide

## Overview

This guide provides step-by-step instructions for migrating from the deprecated `ast_arena` monolithic design to the new `ast_hierarchical_factory` distributed architecture.

## Why Migrate?

### Current Arena Problems
- **Memory Pressure**: Large contiguous allocations cause system-wide pressure
- **Tight Coupling**: All components depend on global arena instance
- **Poor Scalability**: O(n) growth based on worst-case AST size
- **No Garbage Collection**: Manual memory management prone to leaks
- **Cache Performance**: Poor locality for scattered access patterns

### Hierarchical Factory Benefits
- **Reference Counting**: Automatic memory management
- **Distributed Storage**: Better memory utilization and cache locality
- **Component Isolation**: Independent testing and optimization
- **Garbage Collection**: Automatic cleanup of unreachable nodes
- **Performance Monitoring**: Built-in statistics and optimization

## Migration Steps

### Step 1: Include New Modules

Replace arena imports with hierarchical factory:

```fortran
! OLD: Arena-based approach
use ast_arena, only: ast_arena_t, create_ast_arena
use ast_factory, only: push_identifier, push_literal

! NEW: Hierarchical factory approach  
use ast_hierarchical_factory, only: hierarchical_factory_t, create_hierarchical_factory
use ast_node_registry, only: registry_stats_t
```

### Step 2: Initialize Factory Instead of Arena

```fortran
! OLD: Arena initialization
type(ast_arena_t) :: arena
arena = create_ast_arena(initial_capacity=512)

! NEW: Factory initialization
type(hierarchical_factory_t) :: factory
factory = create_hierarchical_factory(initial_capacity=128)  ! Smaller initial size
```

### Step 3: Node Creation Pattern Changes

#### Simple Nodes (Identifiers, Literals)

```fortran
! OLD: Arena push pattern
integer :: id_index, lit_index
id_index = push_identifier(arena, "variable_name", line=10, column=5)
lit_index = push_literal(arena, "42", LITERAL_INTEGER, line=10, column=15)

! NEW: Factory creation pattern
integer :: id_node_id, lit_node_id
id_node_id = factory%create_identifier_node("variable_name", line=10, column=5)
lit_node_id = factory%create_literal_node("42", LITERAL_INTEGER, line=10, column=15)
```

#### Complex Nodes (Expressions, Declarations)

```fortran
! OLD: Arena with index management
integer :: assign_index, target_index, value_index
target_index = push_identifier(arena, "x", line=5, column=1)
value_index = push_literal(arena, "10", LITERAL_INTEGER, line=5, column=5)
assign_index = push_assignment(arena, target_index, value_index, line=5, column=1)

! NEW: Factory with reference validation
integer :: assign_id, target_id, value_id
target_id = factory%create_identifier_node("x", line=5, column=1)
value_id = factory%create_literal_node("10", LITERAL_INTEGER, line=5, column=5)
assign_id = factory%create_assignment_node(target_id, value_id, line=5, column=1)
! Note: Factory automatically manages parent-child references
```

### Step 4: Node Access Pattern Changes

#### Getting Node Data

```fortran
! OLD: Arena direct access
class(ast_node), allocatable :: node
if (node_index > 0 .and. node_index <= arena%size) then
    if (allocated(arena%entries(node_index)%node)) then
        allocate(node, source=arena%entries(node_index)%node)
    end if
end if

! NEW: Factory reference management
class(ast_node), allocatable :: node
node = factory%get_node_reference(node_id)  ! Automatic bounds checking
```

#### Tree Traversal

```fortran
! OLD: Manual index-based traversal
integer, allocatable :: children(:)
children = arena%get_children(parent_index)
do i = 1, size(children)
    call process_child(arena, children(i))
end do

! NEW: Reference-based traversal with validation
! (Implementation depends on specific node type and structure)
class(ast_node), allocatable :: parent_node
parent_node = factory%get_node_reference(parent_id)
select type(parent_node)
type is (program_node)
    if (allocated(parent_node%body_indices)) then
        do i = 1, size(parent_node%body_indices)
            call process_child_node(factory, parent_node%body_indices(i))
        end do
    end if
end select
```

### Step 5: Memory Management Updates

#### Reference Lifecycle

```fortran
! OLD: Manual arena cleanup
call arena%clear()  ! Clears everything

! NEW: Automatic reference counting
call factory%release_node_reference(old_node_id)  ! Decrements ref count
call factory%collect_garbage()                     ! Cleans unreachable nodes
```

#### Root Node Management

```fortran
! OLD: Arena tracks current position
arena%current_index = root_index

! NEW: Factory explicit root management
call factory%set_root_node(root_node_id)
root_node = factory%get_root_node()
```

### Step 6: Error Handling Improvements

#### Invalid Reference Handling

```fortran
! OLD: Manual bounds checking required
if (node_index <= 0 .or. node_index > arena%size) then
    ! Handle error manually
    return
end if

! NEW: Automatic validation with error placeholders
node = factory%get_node_reference(node_id)
if (.not. allocated(node)) then
    ! Factory already handled invalid reference
    ! Error placeholder may have been created
end if
```

### Step 7: Performance Monitoring

#### Statistics and Optimization

```fortran
! OLD: Limited arena statistics
type(ast_arena_stats_t) :: arena_stats
arena_stats = arena%get_stats()
print *, "Nodes:", arena_stats%total_nodes
print *, "Memory:", arena_stats%memory_usage

! NEW: Comprehensive factory statistics
type(factory_stats_t) :: factory_stats
factory_stats = factory%get_memory_stats()
print *, "Created:", factory_stats%nodes_created
print *, "Reused:", factory_stats%nodes_reused
print *, "GC Cycles:", factory_stats%gc_cycles
print *, "Memory Efficiency:", factory_stats%memory_efficiency

! Optimize memory when needed
call factory%optimize_memory()
```

## Common Migration Patterns

### Pattern 1: Simple AST Construction

```fortran
! OLD: Arena-based construction
subroutine build_expression_old(arena, result_index)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(out) :: result_index
    
    integer :: left_idx, right_idx
    left_idx = push_identifier(arena, "x")
    right_idx = push_literal(arena, "42", LITERAL_INTEGER)
    result_index = push_binary_op(arena, left_idx, right_idx, "+")
end subroutine

! NEW: Factory-based construction
subroutine build_expression_new(factory, result_id)
    type(hierarchical_factory_t), intent(inout) :: factory
    integer, intent(out) :: result_id
    
    integer :: left_id, right_id
    left_id = factory%create_identifier_node("x")
    right_id = factory%create_literal_node("42", LITERAL_INTEGER)
    result_id = factory%create_binary_op_node(left_id, right_id, "+")
    ! References automatically managed
end subroutine
```

### Pattern 2: AST Analysis/Transformation

```fortran
! OLD: Arena-based analysis
subroutine analyze_tree_old(arena, root_index)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: root_index
    
    class(ast_node), allocatable :: node
    if (root_index > 0 .and. root_index <= arena%size) then
        allocate(node, source=arena%entries(root_index)%node)
        call analyze_node(node)
    end if
end subroutine

! NEW: Factory-based analysis
subroutine analyze_tree_new(factory, root_id)
    type(hierarchical_factory_t), intent(in) :: factory
    integer, intent(in) :: root_id
    
    class(ast_node), allocatable :: node
    node = factory%get_node_reference(root_id)
    if (allocated(node)) then
        call analyze_node(node)
    end if
end subroutine
```

### Pattern 3: Memory-Intensive Operations

```fortran
! OLD: Arena grows indefinitely
subroutine process_large_file_old(arena, filename)
    type(ast_arena_t), intent(inout) :: arena
    character(len=*), intent(in) :: filename
    
    ! Parse large file - arena keeps growing
    call parse_file(arena, filename)
    ! No automatic cleanup
end subroutine

! NEW: Factory with garbage collection
subroutine process_large_file_new(factory, filename)
    type(hierarchical_factory_t), intent(inout) :: factory
    character(len=*), intent(in) :: filename
    
    ! Parse large file with automatic cleanup
    call parse_file(factory, filename)
    
    ! Trigger garbage collection periodically
    call factory%collect_garbage()
    
    ! Optimize memory usage
    call factory%optimize_memory()
end subroutine
```

## Testing Migration

### Unit Test Updates

```fortran
! OLD: Arena-based test
subroutine test_assignment_old()
    type(ast_arena_t) :: arena
    integer :: target_idx, value_idx, assign_idx
    
    arena = create_ast_arena()
    target_idx = push_identifier(arena, "x")
    value_idx = push_literal(arena, "42", LITERAL_INTEGER)
    assign_idx = push_assignment(arena, target_idx, value_idx)
    
    ! Test assignment properties
    call assert_assignment_valid(arena, assign_idx)
end subroutine

! NEW: Factory-based test
subroutine test_assignment_new()
    type(hierarchical_factory_t) :: factory
    integer :: target_id, value_id, assign_id
    
    factory = create_hierarchical_factory()
    target_id = factory%create_identifier_node("x")
    value_id = factory%create_literal_node("42", LITERAL_INTEGER)
    assign_id = factory%create_assignment_node(target_id, value_id)
    
    ! Test assignment properties with reference validation
    call assert_assignment_valid(factory, assign_id)
end subroutine
```

## Performance Considerations

### Memory Usage
- **Arena**: 256 nodes × 64 bytes = 16KB minimum, grows by 64KB chunks
- **Factory**: 64 nodes × 64 bytes = 4KB minimum, grows by 2KB chunks
- **Garbage Collection**: Factory automatically reclaims unused nodes

### Cache Performance
- **Arena**: Good for sequential access, poor for scattered access
- **Factory**: Better cache locality due to smaller allocations

### Parallelization
- **Arena**: Single global arena prevents parallel processing
- **Factory**: Multiple factory instances enable parallel AST processing

## Troubleshooting

### Common Issues

**Invalid Node References**
```fortran
! Problem: Using deallocated node reference
node = factory%get_node_reference(old_id)
! Solution: Check if node is allocated
if (.not. allocated(node)) then
    ! Handle invalid reference case
end if
```

**Memory Leaks**
```fortran
! Problem: Not releasing node references
call factory%create_temporary_node(...)
! Solution: Explicit cleanup or garbage collection
call factory%collect_garbage()
```

**Performance Issues**
```fortran
! Problem: Frequent allocations without cleanup
do i = 1, large_number
    temp_id = factory%create_literal_node(...)
    ! Use temp node
end do
! Solution: Periodic garbage collection
if (mod(i, 1000) == 0) call factory%collect_garbage()
```

## Migration Checklist

- [ ] Replace arena imports with factory imports
- [ ] Update initialization to use factory instead of arena  
- [ ] Convert node creation from push_* to create_*_node
- [ ] Update node access to use get_node_reference
- [ ] Add proper reference management for parent-child relationships
- [ ] Update error handling to use factory validation
- [ ] Add garbage collection calls for long-running operations
- [ ] Update unit tests to use factory pattern
- [ ] Performance test with factory statistics
- [ ] Remove old arena dependencies

## Benefits After Migration

1. **Automatic Memory Management**: Reference counting eliminates manual cleanup
2. **Better Performance**: Improved cache locality and reduced memory pressure
3. **Component Independence**: Factory can be tested and optimized separately
4. **Error Resilience**: Automatic validation prevents common reference errors
5. **Scalability**: Incremental growth and garbage collection support larger ASTs
6. **Monitoring**: Built-in statistics enable performance optimization

The hierarchical factory architecture provides a robust foundation for future fortfront development while maintaining compatibility during the migration period.