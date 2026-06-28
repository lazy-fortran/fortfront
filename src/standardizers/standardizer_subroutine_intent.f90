module standardizer_subroutine_intent
    use ast_arena_modern, only: ast_arena_t
    use standardizer_parameter, only: param_metadata_t, metadata_find_param
    use standardizer_parameter, only: node_exists
    implicit none
    private
    public :: infer_subroutine_parameter_intents

contains

    subroutine infer_subroutine_parameter_intents(arena, body_indices, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(param_metadata_t), intent(inout) :: metadata
        logical, allocatable :: is_read(:), is_written(:)
        integer :: i, n_params

        if (.not. allocated(metadata%names)) return
        n_params = size(metadata%names)
        if (n_params == 0) return

        allocate (is_read(n_params), is_written(n_params))
        is_read = .false.
        is_written = .false.

        do i = 1, size(body_indices)
            call scan_statement(arena, body_indices(i), metadata, &
                is_read, is_written)
        end do

        do i = 1, n_params
            if (metadata%is_procedure(i)) cycle
            if (len_trim(metadata%intent(i)) > 0) cycle

            if (is_written(i) .and. is_read(i)) then
                metadata%intent(i) = "inout"
            else if (is_written(i)) then
                metadata%intent(i) = "inout"
            else if (is_read(i)) then
                metadata%intent(i) = "in"
            else
                metadata%intent(i) = "inout"
            end if
        end do
    end subroutine infer_subroutine_parameter_intents

    recursive subroutine scan_statement(arena, node_idx, metadata, &
            is_read, is_written)
        use ast_nodes_core, only: assignment_node
        use ast_nodes_io, only: print_statement_node, write_statement_node
        use ast_nodes_io, only: read_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(inout) :: is_read(:), is_written(:)
        integer :: i

        if (.not. node_exists(arena, node_idx)) return

        select type (node => arena%entries(node_idx)%node)
            type is (assignment_node)
            ! Skip keyword arguments - they don't modify variables
            if (.not. node%is_keyword_argument) then
                call mark_lhs_written(arena, node%target_index, metadata, &
                    is_read, is_written)
                call mark_rhs_read(arena, node%value_index, metadata, is_read)
            else
                ! For keyword arguments, only the RHS (value) is read
                call mark_rhs_read(arena, node%value_index, metadata, is_read)
            end if
            type is (print_statement_node)
            if (allocated(node%expression_indices)) then
                do i = 1, size(node%expression_indices)
                    call mark_rhs_read(arena, node%expression_indices(i), &
                        metadata, is_read)
                end do
            end if
            type is (write_statement_node)
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call mark_rhs_read(arena, node%arg_indices(i), &
                        metadata, is_read)
                end do
            end if
            type is (read_statement_node)
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call mark_lhs_written(arena, node%var_indices(i), &
                        metadata, is_read, is_written)
                end do
            end if
        class default
            call scan_children_generic(arena, node_idx, metadata, &
                is_read, is_written)
        end select
    end subroutine scan_statement

    recursive subroutine mark_lhs_written(arena, node_idx, metadata, &
            is_read, is_written)
        use ast_nodes_core, only: identifier_node, call_or_subscript_node
        use ast_nodes_core, only: component_access_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(inout) :: is_read(:), is_written(:)
        integer :: param_idx, i
        character(len=:), allocatable :: name

        if (.not. node_exists(arena, node_idx)) return

        select type (node => arena%entries(node_idx)%node)
            type is (identifier_node)
            if (.not. allocated(node%name)) return
            name = trim(node%name)
            param_idx = metadata_find_param(metadata, name)
            if (param_idx > 0) is_written(param_idx) = .true.
            type is (call_or_subscript_node)
            if (allocated(node%name)) then
                name = trim(node%name)
                param_idx = metadata_find_param(metadata, name)
                if (param_idx > 0) then
                    is_written(param_idx) = .true.
                    if (allocated(node%arg_indices)) then
                        do i = 1, size(node%arg_indices)
                            call mark_rhs_read(arena, node%arg_indices(i), &
                                metadata, is_read)
                        end do
                    end if
                end if
            else if (node%base_expr_index > 0) then
                call mark_lhs_written(arena, node%base_expr_index, metadata, &
                    is_read, is_written)
            end if
            type is (component_access_node)
            if (node%base_expr_index > 0) then
                call mark_lhs_written(arena, node%base_expr_index, metadata, &
                    is_read, is_written)
            end if
        end select
    end subroutine mark_lhs_written

    recursive subroutine mark_rhs_read(arena, node_idx, metadata, is_read)
        use ast_nodes_core, only: identifier_node, binary_op_node
        use ast_nodes_core, only: call_or_subscript_node
        use ast_nodes_core, only: component_access_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(inout) :: is_read(:)
        integer :: param_idx, i
        character(len=:), allocatable :: name

        if (.not. node_exists(arena, node_idx)) return

        select type (node => arena%entries(node_idx)%node)
            type is (identifier_node)
            if (.not. allocated(node%name)) return
            name = trim(node%name)
            param_idx = metadata_find_param(metadata, name)
            if (param_idx > 0) is_read(param_idx) = .true.
            type is (binary_op_node)
            call mark_rhs_read(arena, node%left_index, metadata, is_read)
            call mark_rhs_read(arena, node%right_index, metadata, is_read)
            type is (call_or_subscript_node)
            if (allocated(node%name)) then
                name = trim(node%name)
                param_idx = metadata_find_param(metadata, name)
                if (param_idx > 0) is_read(param_idx) = .true.
            end if
            if (node%base_expr_index > 0) then
                call mark_rhs_read(arena, node%base_expr_index, metadata, is_read)
            end if
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call mark_rhs_read(arena, node%arg_indices(i), metadata, &
                        is_read)
                end do
            end if
            type is (component_access_node)
            if (node%base_expr_index > 0) then
                call mark_rhs_read(arena, node%base_expr_index, metadata, is_read)
            end if
        class default
            call scan_children_for_reads(arena, node_idx, metadata, is_read)
        end select
    end subroutine mark_rhs_read

    subroutine scan_children_generic(arena, node_idx, metadata, &
            is_read, is_written)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(inout) :: is_read(:), is_written(:)
        integer, allocatable :: children(:)
        integer :: i

        children = arena%get_children(node_idx)
        if (size(children) == 0) return
        do i = 1, size(children)
            call scan_statement(arena, children(i), metadata, is_read, is_written)
        end do
    end subroutine scan_children_generic

    subroutine scan_children_for_reads(arena, node_idx, metadata, is_read)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(inout) :: is_read(:)
        integer, allocatable :: children(:)
        integer :: i

        children = arena%get_children(node_idx)
        if (size(children) == 0) return
        do i = 1, size(children)
            call mark_rhs_read(arena, children(i), metadata, is_read)
        end do
    end subroutine scan_children_for_reads

end module standardizer_subroutine_intent
