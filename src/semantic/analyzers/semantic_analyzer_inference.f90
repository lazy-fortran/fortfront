module semantic_analyzer_inference
    ! Type inference functions for semantic analysis
    use scope_manager
    use parameter_tracker
    use expression_temporary_tracker_module
    use error_handling, only: error_collection_t, create_error_collection, result_t, &
                               create_error_result, ERROR_SEMANTIC
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   substitution_t, &
                                   create_mono_type, create_poly_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, &
                                   type_args_allocated, type_args_size, type_args_element
    use ast_core
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
                               assignment_node, call_or_subscript_node, &
                               array_literal_node, program_node
    use ast_nodes_procedure, only: subroutine_call_node
    use ast_nodes_control, only: do_loop_node, if_node, do_while_node, &
                                  where_node, where_stmt_node, forall_node, &
                                  select_case_node, case_block_node, &
                                  associate_node, association_t, cycle_node, exit_node, &
                                  stop_node, return_node, elsewhere_clause_t
    use ast_nodes_data, only: intent_type_to_string, declaration_node, module_node
    use ast_nodes_bounds, only: array_spec_t, array_bounds_t, array_slice_node, &
                                range_expression_node, get_array_slice_node
    implicit none
    private

    public :: semantic_context_t
    public :: infer_statement_type, infer_literal, infer_identifier
    public :: infer_binary_op, infer_function_call, infer_array_slice
    public :: infer_assignment, infer_array_literal, infer_implied_do_loop
    public :: infer_declaration_helper, update_identifier_type_in_arena

    ! Semantic analysis context
    type :: semantic_context_t
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(parameter_tracker_t) :: param_tracker  ! Track parameter attributes
        type(temp_tracker_t) :: temp_tracker  ! Track expression temporaries
        type(error_collection_t) :: errors  ! Collect semantic errors
        logical :: strict_mode = .false.  ! True for standard Fortran (implicit none), false for lazy Fortran
    contains
        procedure :: infer_stmt => infer_statement_type
        procedure :: unify => unify_types
        procedure :: instantiate => instantiate_type_scheme
        procedure :: generalize => generalize_type
        procedure :: fresh_type_var => generate_fresh_type_var
        procedure :: apply_subst_to_type => apply_current_substitution
        procedure :: get_builtin_function_type
        procedure :: compose_with_subst
        procedure :: deep_copy => semantic_context_deep_copy
        procedure :: assign => semantic_context_assign
        procedure :: validate_bounds => validate_array_access_bounds
        procedure :: check_conformance => check_array_shape_conformance
        procedure :: has_errors => semantic_context_has_errors
        generic :: assignment(=) => assign
    end type semantic_context_t

contains

    ! Main statement type inference dispatcher
    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ
        type(ast_entry_t) :: stmt
        
        if (stmt_index <= 0 .or. stmt_index > arena%size) then
            typ = create_mono_type(TVAR, var=this%fresh_type_var())
            return
        end if
        
        stmt = arena%nodes(stmt_index)
        
        select case (stmt%node_type)
        case (literal_node%type_id)
            typ = infer_literal(this, stmt%literal_data)
        case (identifier_node%type_id)
            typ = infer_identifier(this, stmt%identifier_data)
        case (binary_op_node%type_id)
            typ = infer_binary_op(this, arena, stmt%binary_op_data, stmt_index)
        case (assignment_node%type_id)
            typ = infer_assignment(this, arena, stmt%assignment_data, stmt_index)
        case (call_or_subscript_node%type_id)
            typ = infer_function_call(this, arena, stmt%call_or_subscript_data)
        case (array_literal_node%type_id)
            typ = infer_array_literal(this, arena, stmt%array_literal_data, stmt_index)
        case (declaration_node%type_id)
            typ = infer_declaration_helper(this, stmt%declaration_data)
        case (if_node%type_id)
            typ = infer_if_helper(this, arena, stmt)
        case (do_while_node%type_id)
            typ = infer_do_while_helper(this, arena, stmt)
        case (where_node%type_id)
            typ = infer_where_helper(this, arena, stmt)
        case (where_stmt_node%type_id)
            typ = infer_where_stmt_helper(this, arena, stmt)
        case (forall_node%type_id)
            typ = infer_forall_helper(this, arena, stmt)
        case (select_case_node%type_id)
            typ = infer_select_case_helper(this, arena, stmt)
        case (associate_node%type_id)
            typ = infer_associate_helper(this, arena, stmt)
        case (stop_node%type_id, exit_node%type_id, cycle_node%type_id, return_node%type_id)
            typ = infer_stop_helper(this, arena, stmt)
        case default
            ! Default case for unhandled node types
            typ = create_mono_type(TVAR, var=this%fresh_type_var())
        end select
    end function infer_statement_type

    ! Infer type of literal
    function infer_literal(ctx, lit) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        ! Use literal_kind for integer comparisons
        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case (LITERAL_STRING)
            ! Calculate character length from string literal (excluding quotes)
            if (allocated(lit%value) .and. len(lit%value) >= 2) then
                typ = create_mono_type(TCHAR, char_size=len(lit%value) - 2)
            else
                typ = create_mono_type(TCHAR, char_size=0)
            end if
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TLOGICAL)
        case default
            typ = create_mono_type(TREAL)
        end select
    end function infer_literal

    ! Infer type of identifier
    function infer_identifier(ctx, ident) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(identifier_node), intent(in) :: ident
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(result_t) :: error_result

        ! Safety check: ensure identifier name is allocated and not empty
        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            return
        end if

        ! Look up identifier in hierarchical scopes
        call ctx%scopes%lookup(ident%name, scheme)

        if (allocated(scheme)) then
            ! Found in environment - instantiate the type scheme
            typ = ctx%instantiate(scheme)
        else
            ! Not found - behavior depends on mode
            if (ctx%strict_mode) then
                ! Standard Fortran mode: undefined variable is an error
                error_result = create_error_result( &
                    "Undefined variable '" // ident%name // "'", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="infer_identifier", &
                    suggestion="Declare the variable before using it" &
                )
                call ctx%errors%add_result(error_result)
                
                ! Create fresh type variable for continued analysis
                typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            else
                ! Lazy Fortran mode: auto-declare undefined variables
                ! Create a fresh type variable and add to scope for future use
                typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
                
                ! Create polymorphic type scheme (no generalization needed for simple variables)
                block
                    type(poly_type_t) :: new_scheme
                    new_scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
                    call ctx%scopes%define(ident%name, new_scheme)
                end block
            end if
        end if
    end function infer_identifier

    ! Infer type of binary operation (simplified)
    function infer_binary_op(ctx, arena, binop, binop_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(in) :: binop
        integer, intent(in) :: binop_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ

        ! Infer types of operands
        left_typ = ctx%infer_stmt(arena, binop%left_index)
        right_typ = ctx%infer_stmt(arena, binop%right_index)

        ! Special handling for string concatenation
        if (binop%operator == "//") then
            ! Calculate combined string length if both operands are known
            block
                integer :: left_size, right_size, total_size
                logical :: can_calculate_size
                
                ! Try to get sizes of operands
                left_size = 0
                right_size = 0
                can_calculate_size = .false.
                
                ! Get left operand size
                if (left_typ%kind == TCHAR .and. left_typ%size >= 0) then
                    left_size = left_typ%size
                end if
                
                ! Get right operand size
                if (right_typ%kind == TCHAR .and. right_typ%size >= 0) then
                    right_size = right_typ%size
                end if
                
                ! If we can determine both sizes, calculate total
                if (left_typ%kind == TCHAR .and. right_typ%kind == TCHAR .and. &
                    left_typ%size >= 0 .and. right_typ%size >= 0) then
                    total_size = left_size + right_size
                    can_calculate_size = .true.
                end if
                
                ! Create appropriate character type
                if (can_calculate_size) then
                    typ = create_mono_type(TCHAR, char_size=total_size)
                    typ%alloc_info%needs_allocatable_string = .false.
                else
                    typ = create_mono_type(TCHAR)
                    typ%alloc_info%needs_allocatable_string = .true.
                end if
            end block
            
            call ctx%unify(left_typ, create_mono_type(TCHAR))
            call ctx%unify(right_typ, create_mono_type(TCHAR))
        ! Comparison operators return logical
        else if (binop%operator == "==" .or. binop%operator == "/=" .or. &
                 binop%operator == "<" .or. binop%operator == "<=" .or. &
                 binop%operator == ">" .or. binop%operator == ">=") then
            call ctx%unify(left_typ, right_typ)
            typ = create_mono_type(TLOGICAL)
        ! Logical operators
        else if (binop%operator == ".and." .or. binop%operator == ".or." .or. &
                 binop%operator == ".not." .or. binop%operator == ".eqv." .or. &
                 binop%operator == ".neqv.") then
            typ = create_mono_type(TLOGICAL)
            call ctx%unify(left_typ, typ)
            call ctx%unify(right_typ, typ)
        ! Arithmetic operators preserve type
        else
            call ctx%unify(left_typ, right_typ)
            typ = left_typ
        end if

        ! Store inferred type in node if it's a binary_op_node
        arena%entries(binop_index)%node%inferred_type = typ
    end function infer_binary_op

    ! Infer type of function call (simplified)
    function infer_function_call(ctx, arena, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: arg_type
        integer :: i

        ! Process arguments to detect undefined variables
        if (allocated(call_node%arg_indices)) then
            do i = 1, size(call_node%arg_indices)
                if (call_node%arg_indices(i) > 0 .and. &
                    call_node%arg_indices(i) <= arena%size) then
                    ! Infer argument type (this will detect undefined variables)
                    arg_type = ctx%infer_stmt(arena, call_node%arg_indices(i))
                end if
            end do
        end if

        ! Look up function in scope
        call ctx%scopes%lookup(call_node%name, scheme)
        
        if (allocated(scheme)) then
            typ = ctx%instantiate(scheme)
            ! Extract return type from function type
            if (typ%kind == TFUN .and. type_args_allocated(typ) .and. type_args_size(typ) >= 2) then
                typ = type_args_element(typ, 2)  ! Second arg is return type
            end if
        else
            ! Unknown function - default to real type
            typ = create_mono_type(TREAL)
        end if
    end function infer_function_call

    ! Infer type of array slice
    function infer_array_slice(ctx, arena, slice_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        type(mono_type_t) :: typ
        
        ! For now, return real array type
        typ = create_mono_type(TARRAY)
    end function infer_array_slice

    ! Infer type of assignment with hierarchical scopes
    function infer_assignment(ctx, arena, assignment, assignment_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: expr_typ, existing_typ
        type(poly_type_t), allocatable :: scheme, existing_scheme
        integer :: lhs_index
        type(result_t) :: error_result

        lhs_index = assignment%target_index
        expr_typ = ctx%infer_stmt(arena, assignment%value_index)

        if (lhs_index > 0 .and. lhs_index <= arena%size) then
            if (allocated(arena%entries(lhs_index)%node)) then
                select type (lhs_node => arena%entries(lhs_index)%node)
                type is (identifier_node)
                    ! Check if already defined in current or parent scope
                    call ctx%scopes%lookup(lhs_node%name, existing_scheme)
                    
                    if (allocated(existing_scheme)) then
                        ! Variable exists - unify with existing type
                        existing_typ = ctx%instantiate(existing_scheme)
                        call ctx%unify(existing_typ, expr_typ)
                    else
                        ! Assignment to undefined variable - behavior depends on mode
                        if (ctx%strict_mode) then
                            ! Standard Fortran mode: undefined variable is an error
                            error_result = create_error_result( &
                                "Undefined variable '" // lhs_node%name // "' in assignment", &
                                ERROR_SEMANTIC, &
                                component="semantic_analyzer", &
                                context="infer_assignment", &
                                suggestion="Declare the variable before assigning to it" &
                            )
                            call ctx%errors%add_result(error_result)
                        end if
                        
                        ! Continue analysis with inferred type (both modes)
                        expr_typ = ctx%apply_subst_to_type(expr_typ)
                    end if
                    
                    ! Handle allocatable character detection only when size cannot be determined
                    if (expr_typ%kind == TCHAR) then
                        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
                            if (allocated(arena%entries(assignment%value_index)%node)) then
                                select type (value_node => arena%entries(assignment%value_index)%node)
                                type is (binary_op_node)
                                    if (value_node%operator == "//") then
                                        ! Only mark as allocatable if size was not calculated
                                        if (expr_typ%size < 0) then
                                            expr_typ%alloc_info%is_allocatable = .true.
                                            expr_typ%alloc_info%needs_allocatable_string = .true.
                                            expr_typ%size = 0  ! Deferred length
                                        end if
                                        
                                        ! Update all existing identifier nodes with this name
                                        call update_identifier_type_in_arena(arena, lhs_node%name, expr_typ)
                                    end if
                                end select
                            end if
                        end if
                    end if
                    
                    ! Update all identifier nodes in the arena with the inferred type
                    call update_identifier_type_in_arena(arena, lhs_node%name, expr_typ)
                    
                    ! Generalize the expression type and define/update in scope
                    allocate(scheme)
                    scheme = ctx%generalize(expr_typ)
                    call ctx%scopes%define(lhs_node%name, scheme)
                end select
            end if
        end if

        ! For array assignments, return the element type instead of array type
        ! This helps with type inference tests that expect element types
        if (expr_typ%kind == TARRAY .and. expr_typ%get_args_count() > 0) then
            typ = expr_typ%get_arg(1)  ! Return element type
        else
            typ = expr_typ
        end if

        ! Store the actual assignment type
        arena%entries(assignment_index)%node%inferred_type = typ
    end function infer_assignment

    ! Helper: Update identifier type throughout arena
    subroutine update_identifier_type_in_arena(arena, name, new_type)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: new_type
        integer :: i

        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (identifier_node)
                    if (node%name == name) then
                        arena%entries(i)%node%inferred_type = new_type
                    end if
                end select
            end if
        end do
    end subroutine update_identifier_type_in_arena

    ! Placeholder implementations for other inference functions
    ! These will be moved from the control module once we extract it

    function infer_array_literal(ctx, arena, array_lit, array_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        integer, intent(in) :: array_index
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TARRAY)
    end function infer_array_literal

    function infer_implied_do_loop(ctx, arena, do_loop, do_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(in) :: do_loop
        integer, intent(in) :: do_index
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TARRAY)
    end function infer_implied_do_loop

    function infer_declaration_helper(ctx, decl) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(declaration_node), intent(in) :: decl
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TREAL)
    end function infer_declaration_helper

    function infer_if_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TLOGICAL)
    end function infer_if_helper

    function infer_do_while_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TLOGICAL)
    end function infer_do_while_helper

    function infer_where_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TARRAY)
    end function infer_where_helper

    function infer_where_stmt_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TARRAY)
    end function infer_where_stmt_helper

    function infer_forall_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TARRAY)
    end function infer_forall_helper

    function infer_select_case_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TLOGICAL)
    end function infer_select_case_helper

    function infer_associate_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TREAL)
    end function infer_associate_helper

    function infer_stop_helper(ctx, arena, node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: node
        type(mono_type_t) :: typ
        
        typ = create_mono_type(TLOGICAL)
    end function infer_stop_helper

end module semantic_analyzer_inference