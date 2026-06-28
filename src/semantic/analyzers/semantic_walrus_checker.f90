module semantic_walrus_checker
    ! Walrus declaration redeclaration checking (LFortran extension).
    ! A `name := expr` statement declares `name` and infers its type. Per the
    ! LFortran Infer spec, redeclaring a name in the same scope is an error,
    ! while inner scopes may shadow outer names. This checker walks each scope
    ! body, tracks names introduced by walrus declarations and ordinary
    ! declarations, and reports same-scope redeclarations.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, identifier_node, program_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t, &
        error_collection_t
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none
    private

    public :: check_walrus_redeclaration

contains

    subroutine check_walrus_redeclaration(errors, input_mode, arena, prog_index)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: input_mode
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index

        if (input_mode == INPUT_MODE_STANDARD) return
        if (.not. arena%has_node_at(prog_index)) return

        call check_scope_node(errors, arena, prog_index)
    end subroutine check_walrus_redeclaration

    ! Check one scope node (program/procedure) and recurse into nested procedure
    ! scopes. Each scope starts with a fresh name set so inner scopes may shadow.
    ! BLOCK/ASSOCIATE bodies are not descended into: the lazy frontend flattens
    ! them into the enclosing body, so their statements cannot be reliably
    ! attributed to a distinct scope here.
    recursive subroutine check_scope_node(errors, arena, node_index)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            if (allocated(node%body_indices)) &
                call check_scope_body(errors, arena, node%body_indices)
            type is (function_def_node)
            if (allocated(node%body_indices)) &
                call check_scope_body(errors, arena, node%body_indices)
            type is (subroutine_def_node)
            if (allocated(node%body_indices)) &
                call check_scope_body(errors, arena, node%body_indices)
        end select
    end subroutine check_scope_node

    subroutine check_scope_body(errors, arena, body_indices)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=:), allocatable :: declared_names(:)
        integer :: name_count, i, stmt_index

        allocate (character(len=64) :: declared_names(max(1, size(body_indices))))
        name_count = 0

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle
            call check_scope_statement(errors, arena, stmt_index, &
                declared_names, name_count)
        end do
    end subroutine check_scope_body

    recursive subroutine check_scope_statement(errors, arena, stmt_index, &
            declared_names, name_count)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        character(len=*), intent(inout) :: declared_names(:)
        integer, intent(inout) :: name_count

        select type (stmt => arena%entries(stmt_index)%node)
            type is (declaration_node)
            call register_declaration_names(stmt, declared_names, name_count)
            type is (assignment_node)
            if (stmt%is_walrus) &
                call register_walrus_name(errors, arena, stmt, declared_names, &
                name_count)
            type is (function_def_node)
            call check_scope_node(errors, arena, stmt_index)
            type is (subroutine_def_node)
            call check_scope_node(errors, arena, stmt_index)
        end select
    end subroutine check_scope_statement

    subroutine register_declaration_names(decl, declared_names, name_count)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(inout) :: declared_names(:)
        integer, intent(inout) :: name_count
        integer :: j

        if (allocated(decl%var_names)) then
            do j = 1, size(decl%var_names)
                call add_name(declared_names, name_count, trim(decl%var_names(j)))
            end do
        else if (allocated(decl%var_name)) then
            call add_name(declared_names, name_count, trim(decl%var_name))
        end if
    end subroutine register_declaration_names

    subroutine register_walrus_name(errors, arena, stmt, declared_names, name_count)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=*), intent(inout) :: declared_names(:)
        integer, intent(inout) :: name_count
        character(len=:), allocatable :: target_name
        type(result_t) :: error_result

        target_name = walrus_target_name(arena, stmt%target_index)
        if (len_trim(target_name) == 0) return

        if (name_exists(declared_names, name_count, target_name)) then
            error_result = create_error_result( &
                "Walrus ':=' redeclares '"//target_name// &
                "' in the same scope", &
                ERROR_SEMANTIC, &
                component="semantic_walrus_checker", &
                context="check_walrus_redeclaration", &
                suggestion="Use '=' to assign to an existing variable" &
                )
            call errors%add_result(error_result)
            return
        end if

        call add_name(declared_names, name_count, target_name)
    end subroutine register_walrus_name

    function walrus_target_name(arena, target_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        character(len=:), allocatable :: name

        name = ""
        if (.not. arena%has_node_at(target_index)) return
        select type (target => arena%entries(target_index)%node)
            type is (identifier_node)
            if (allocated(target%name)) name = trim(target%name)
        end select
    end function walrus_target_name

    subroutine add_name(declared_names, name_count, name)
        character(len=*), intent(inout) :: declared_names(:)
        integer, intent(inout) :: name_count
        character(len=*), intent(in) :: name

        if (name_count >= size(declared_names)) return
        if (name_exists(declared_names, name_count, name)) return
        name_count = name_count + 1
        declared_names(name_count) = name
    end subroutine add_name

    logical function name_exists(declared_names, name_count, name) result(found)
        character(len=*), intent(in) :: declared_names(:)
        integer, intent(in) :: name_count
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        do i = 1, name_count
            if (trim(declared_names(i)) == name) then
                found = .true.
                return
            end if
        end do
    end function name_exists

end module semantic_walrus_checker
