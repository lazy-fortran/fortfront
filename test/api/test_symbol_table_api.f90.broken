program test_symbol_table_api
    use fortfront
    use type_system_unified, only: create_fun_type, create_mono_type, create_poly_type, &
                                  type_var_t, TINT, TREAL
    implicit none
    
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: ctx
    type(symbol_info_t) :: symbol
    type(symbol_info_t), allocatable :: symbols(:)
    type(symbol_reference_t), allocatable :: references(:)
    type(scope_info_t), allocatable :: scope_infos(:)
    integer :: i
    
    print *, "=== Symbol Table API Test ==="
    
    ! Create arena and semantic context
    arena = create_ast_arena()
    ctx = create_semantic_context()
    
    ! Create test scenario: simple program with variables
    call create_test_program(arena, ctx)
    
    ! Test symbol lookup
    print *, "Testing get_symbol_info..."
    symbol = get_symbol_info(ctx, "x")
    if (symbol%name /= "") then
        print *, "  ✓ Found symbol:", symbol%name
        print *, "  ✓ Definition line:", symbol%definition_line
        print *, "  ✓ Definition column:", symbol%definition_column
        print *, "  ✓ Is parameter:", symbol%is_parameter
        print *, "  ✓ Is used:", symbol%is_used
    else
        print *, "  ✗ Symbol 'x' not found"
    end if
    
    ! Test scope symbol listing
    print *, "Testing get_symbols_in_scope..."
    if (ctx%scopes%depth > 0) then
        symbols = get_symbols_in_scope(ctx, 1)  ! Get symbols from first scope
        print *, "  ✓ Found", size(symbols), "symbols in scope 1"
        do i = 1, min(size(symbols), 3)  ! Show first 3 symbols
            print *, "    Symbol", i, ":", symbols(i)%name
        end do
    else
        print *, "  ✗ No scopes available"
    end if
    
    ! Test symbol references
    print *, "Testing get_symbol_references..."
    references = get_symbol_references(arena, ctx, "x")
    print *, "  ✓ Found", size(references), "references to 'x'"
    do i = 1, min(size(references), 2)  ! Show first 2 references
        print *, "    Reference", i, "at node:", references(i)%node_index
    end do
    
    ! Test scope information
    print *, "Testing get_all_scopes..."
    scope_infos = get_all_scopes(ctx)
    print *, "  ✓ Found", size(scope_infos), "scopes"
    do i = 1, size(scope_infos)
        print *, "    Scope", i, ": level=", scope_infos(i)%level, &
                 "type=", scope_infos(i)%scope_type, &
                 "symbols=", scope_infos(i)%symbol_count
    end do
    
    ! Test error handling
    print *, "Testing error handling..."
    symbol = get_symbol_info(ctx, "nonexistent_symbol")
    if (symbol%name == "") then
        print *, "  ✓ Correctly handled missing symbol"
    else
        print *, "  ✗ Should not have found nonexistent symbol"
    end if
    
    print *, "All symbol table API tests completed!"
    
contains
    
    subroutine create_test_program(arena, ctx)
        type(ast_arena_t), intent(inout) :: arena
        type(semantic_context_t), intent(inout) :: ctx
        type(program_node) :: prog
        type(declaration_node) :: decl
        type(identifier_node) :: id1, id2
        type(literal_node) :: lit
        type(assignment_node) :: assign
        
        ! Enter global scope  
        call ctx%scopes%enter_block()
        
        ! Create variable declarations: integer :: x, y
        id1%name = "x"
        call arena%push(id1, "identifier")
        
        id2%name = "y" 
        call arena%push(id2, "identifier")
        
        ! Add symbols to scope
        call add_test_symbols(ctx)
        
        ! Create assignment: x = 42
        lit%value = "42"
        lit%literal_type = "integer"
        call arena%push(lit, "literal")
        
        assign%target_index = 1  ! x
        assign%value_index = 3   ! 42
        assign%operator = "="
        call arena%push(assign, "assignment")
        
        ! Create program node
        prog%name = "test_program"
        allocate(prog%body_indices(1))
        prog%body_indices(1) = 4  ! assignment
        call arena%push(prog, "program")
    end subroutine create_test_program
    
    subroutine add_test_symbols(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t) :: int_type
        type(poly_type_t) :: int_scheme
        
        ! Create integer type
        int_type = create_mono_type(TINT)
        block
            type(type_var_t) :: empty_forall(0)
            int_scheme = create_poly_type(empty_forall, int_type)
        end block
        
        ! Add test symbols to current scope
        call ctx%scopes%define("x", int_scheme)
        call ctx%scopes%define("y", int_scheme)
        
        ! Add some builtin functions for testing
        call add_builtin_functions(ctx)
    end subroutine add_test_symbols
    
    subroutine add_builtin_functions(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t) :: real_type, fun_type
        type(poly_type_t) :: sin_scheme
        
        ! Create real -> real function type for sin using unified arena API
        real_type = create_mono_type(TREAL)
        fun_type = create_fun_type(real_type, real_type)  ! real -> real
        
        block
            type(type_var_t) :: empty_forall(0)
            sin_scheme = create_poly_type(empty_forall, fun_type)
        end block
        call ctx%scopes%define("sin", sin_scheme)
    end subroutine add_builtin_functions
    
end program test_symbol_table_api