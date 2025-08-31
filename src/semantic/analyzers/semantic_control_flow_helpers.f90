module semantic_control_flow_helpers
    ! Control flow type inference helpers
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, create_mono_type, &
                                   create_type_var, create_poly_type
    use ast_core, only: ast_arena_t
    use ast_nodes_control, only: if_node, do_while_node, where_node, where_stmt_node, &
                                 forall_node, select_case_node, associate_node, stop_node
    use ast_nodes_data, only: declaration_node
    use semantic_context_types, only: semantic_context_base_t
    use semantic_inference_helpers, only: process_if_node_branches, process_do_while_node_body, &
                                          process_where_node_clauses, process_where_stmt_node, &
                                          process_forall_node_body, process_select_case_blocks, &
                                          process_associate_node_body, process_stop_node_code, &
                                          process_declaration_variables
    implicit none
    private

    public :: infer_declaration_helper, infer_if_helper, infer_do_while_helper
    public :: infer_where_helper, infer_where_stmt_helper, infer_forall_helper
    public :: infer_select_case_helper, infer_associate_helper, infer_stop_helper

    ! Forward declaration for semantic_context_t (to avoid circular dependency)
    type, abstract, extends(semantic_context_base_t) :: semantic_context_extended_t
    contains
        procedure(generalize_interface), deferred :: generalize
        procedure(enter_block_interface), deferred :: enter_block
        procedure(leave_scope_interface), deferred :: leave_scope
        procedure(define_interface), deferred :: define
        procedure(infer_interface), deferred :: infer
    end type

    abstract interface
        function generalize_interface(this, typ) result(scheme)
            import :: semantic_context_extended_t, mono_type_t, poly_type_t
            class(semantic_context_extended_t), intent(in) :: this
            type(mono_type_t), intent(in) :: typ
            type(poly_type_t) :: scheme
        end function

        subroutine enter_block_interface(this)
            import :: semantic_context_extended_t
            class(semantic_context_extended_t), intent(inout) :: this
        end subroutine

        subroutine leave_scope_interface(this)
            import :: semantic_context_extended_t
            class(semantic_context_extended_t), intent(inout) :: this
        end subroutine

        subroutine define_interface(this, name, scheme)
            import :: semantic_context_extended_t, poly_type_t
            class(semantic_context_extended_t), intent(inout) :: this
            character(len=*), intent(in) :: name
            type(poly_type_t), intent(in) :: scheme
        end subroutine

        function infer_interface(this, arena, index) result(typ)
            import :: semantic_context_extended_t, ast_arena_t, mono_type_t
            class(semantic_context_extended_t), intent(inout) :: this
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: index
            type(mono_type_t) :: typ
        end function
    end interface

contains

    ! Control flow type inference functions
    function infer_declaration_helper(ctx, decl) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(declaration_node), intent(in) :: decl
        type(mono_type_t) :: typ
        type(poly_type_t) :: scheme
        integer :: i
        
        ! Get base type from helper
        call process_declaration_variables(decl, typ)
        
        ! Create type scheme (simplified for base context)
        select type (ctx)
        class is (semantic_context_extended_t)
            scheme = ctx%generalize(typ)
            
            ! Add variables to scope
            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                do i = 1, size(decl%var_names)
                    call ctx%define(decl%var_names(i), scheme)
                end do
            else if (allocated(decl%var_name)) then
                call ctx%define(decl%var_name, scheme)
            end if
        end select
    end function infer_declaration_helper

    function infer_if_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        ! Process control flow with simplified context handling
        select type (ctx)
        class is (semantic_context_extended_t)
            call process_control_flow_node_simple(ctx, arena, node%condition_index, &
                                                  node%then_body_indices, node%else_body_indices)
        end select
        call process_if_node_branches(node, typ)
    end function infer_if_helper

    function infer_do_while_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_while_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        select type (ctx)
        class is (semantic_context_extended_t)
            call process_simple_control_node_helper(ctx, arena, node%condition_index, node%body_indices)
        end select
        call process_do_while_node_body(node, typ)
    end function infer_do_while_helper

    function infer_where_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(where_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        select type (ctx)
        class is (semantic_context_extended_t)
            call process_simple_control_node_helper(ctx, arena, node%mask_expr_index, node%where_body_indices)
        end select
        call process_where_node_clauses(node, typ)
    end function infer_where_helper

    function infer_where_stmt_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(where_stmt_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        select type (ctx)
        class is (semantic_context_extended_t)
            if (node%mask_expr_index > 0) typ = ctx%infer(arena, node%mask_expr_index)
            if (node%assignment_index > 0) then
                typ = ctx%infer(arena, node%assignment_index)
            else
                call process_where_stmt_node(node, typ)
            end if
        class default
            ! Fallback for base context
            call process_where_stmt_node(node, typ)
        end select
    end function infer_where_stmt_helper

    function infer_forall_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(forall_node), intent(in) :: node
        type(mono_type_t) :: typ
        type(mono_type_t) :: temp_type
        type(poly_type_t) :: int_scheme
        integer :: i
        
        ! Get integer scheme and control type
        call process_forall_node_body(node, int_scheme, typ)
        
        select type (ctx)
        class is (semantic_context_extended_t)
            ! Enter new scope
            call ctx%enter_block()
            
            ! Add index variables
            if (allocated(node%index_names)) then
                do i = 1, size(node%index_names)
                    call ctx%define(node%index_names(i), int_scheme)
                end do
            end if
            
            ! Process bounds and body
            if (allocated(node%lower_bound_indices)) then
                do i = 1, size(node%lower_bound_indices)
                    if (node%lower_bound_indices(i) > 0) then
                        temp_type = ctx%infer(arena, node%lower_bound_indices(i))
                    end if
                end do
            end if
            
            if (allocated(node%upper_bound_indices)) then
                do i = 1, size(node%upper_bound_indices)
                    if (node%upper_bound_indices(i) > 0) then
                        temp_type = ctx%infer(arena, node%upper_bound_indices(i))
                    end if
                end do
            end if
            
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    temp_type = ctx%infer(arena, node%body_indices(i))
                end do
            end if
            
            ! Exit scope
            call ctx%leave_scope()
        end select
    end function infer_forall_helper

    function infer_select_case_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(select_case_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        select type (ctx)
        class is (semantic_context_extended_t)
            if (node%selector_index > 0) typ = ctx%infer(arena, node%selector_index)
            call process_indices_array_helper(ctx, arena, node%case_indices)
            if (node%default_index > 0) typ = ctx%infer(arena, node%default_index)
        end select
        call process_select_case_blocks(node, typ)
    end function infer_select_case_helper

    function infer_associate_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(associate_node), intent(in) :: node
        type(mono_type_t) :: typ
        type(mono_type_t) :: assoc_type
        type(poly_type_t) :: assoc_scheme
        integer :: i
        
        select type (ctx)
        class is (semantic_context_extended_t)
            ! Enter new scope
            call ctx%enter_block()
            
            ! Process associations
            if (allocated(node%associations)) then
                do i = 1, size(node%associations)
                    if (node%associations(i)%expr_index > 0) then
                        assoc_type = ctx%infer(arena, node%associations(i)%expr_index)
                        assoc_scheme = create_poly_type(forall_vars=[type_var_t::], mono=assoc_type)
                        if (allocated(node%associations(i)%name)) then
                            call ctx%define(node%associations(i)%name, assoc_scheme)
                        end if
                    end if
                end do
            end if
            
            ! Process body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    assoc_type = ctx%infer(arena, node%body_indices(i))
                end do
            end if
            
            ! Exit scope
            call ctx%leave_scope()
        end select
        
        call process_associate_node_body(node, typ)
    end function infer_associate_helper

    function infer_stop_helper(ctx, arena, node) result(typ)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(stop_node), intent(in) :: node
        type(mono_type_t) :: typ
        
        select type (ctx)
        class is (semantic_context_extended_t)
            if (node%stop_code_index > 0) typ = ctx%infer(arena, node%stop_code_index)
        end select
        call process_stop_node_code(node, typ)
    end function infer_stop_helper

    ! Helper subroutines
    subroutine process_simple_control_node_helper(ctx, arena, condition_index, body_indices)
        class(semantic_context_extended_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        type(mono_type_t) :: temp_type
        integer :: i
        
        if (condition_index > 0) temp_type = ctx%infer(arena, condition_index)
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                temp_type = ctx%infer(arena, body_indices(i))
            end do
        end if
    end subroutine process_simple_control_node_helper

    subroutine process_indices_array_helper(ctx, arena, indices)
        class(semantic_context_extended_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: indices(:)
        type(mono_type_t) :: temp_type
        integer :: i
        
        if (present(indices)) then
            do i = 1, size(indices); temp_type = ctx%infer(arena, indices(i)); end do
        end if
    end subroutine process_indices_array_helper

    subroutine process_control_flow_node_simple(ctx, arena, condition_index, then_indices, else_indices)
        class(semantic_context_extended_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_indices(:), else_indices(:)
        type(mono_type_t) :: temp_type
        
        if (condition_index > 0) temp_type = ctx%infer(arena, condition_index)
        call process_indices_array_helper(ctx, arena, then_indices)
        call process_indices_array_helper(ctx, arena, else_indices)
    end subroutine process_control_flow_node_simple

end module semantic_control_flow_helpers