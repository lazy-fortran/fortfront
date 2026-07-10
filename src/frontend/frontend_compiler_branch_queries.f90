module frontend_compiler_branch_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: goto_node, pause_node, continue_node
    implicit none
    private

    integer, parameter, public :: BRANCH_GOTO = 1
    integer, parameter, public :: BRANCH_PAUSE = 2
    integer, parameter, public :: BRANCH_CONTINUE = 3

    type, public :: branch_target_query_t
        character(len=:), allocatable :: label
        logical :: has_node = .false.
        integer :: node_index = 0
    end type branch_target_query_t

    type, public :: branch_statement_query_t
        logical :: found = .false.
        integer :: statement_kind = 0
        integer :: line = 0
        integer :: column = 0
        logical :: has_statement_label = .false.
        character(len=:), allocatable :: statement_label
        logical :: has_target_label = .false.
        character(len=:), allocatable :: target_label
        logical :: has_target_labels = .false.
        character(len=:), allocatable :: target_labels
        type(branch_target_query_t), allocatable :: targets(:)
        logical :: is_computed = .false.
        logical :: has_selector = .false.
        integer :: selector_node_index = 0
        logical :: has_code = .false.
        integer :: code_node_index = 0
        logical :: has_message = .false.
        character(len=:), allocatable :: message
    end type branch_statement_query_t

    public :: query_branch_statement

contains

    function query_branch_statement(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(branch_statement_query_t) :: query

        call initialize_branch_query(query)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (goto_node)
            call fill_goto_query(arena, node_index, node, query)
            type is (pause_node)
            call fill_pause_query(node, query)
            type is (continue_node)
            query%found = .true.
            query%statement_kind = BRANCH_CONTINUE
        end select
        if (.not. query%found) return
        query%line = arena%entries(node_index)%node%line
        query%column = arena%entries(node_index)%node%column
        call copy_statement_label(arena, node_index, query)
    end function query_branch_statement

    subroutine initialize_branch_query(query)
        type(branch_statement_query_t), intent(out) :: query

        query%statement_label = ''
        query%target_label = ''
        query%target_labels = ''
        query%message = ''
        allocate (query%targets(0))
    end subroutine initialize_branch_query

    subroutine copy_statement_label(arena, node_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(branch_statement_query_t), intent(inout) :: query

        if (.not. allocated(arena%entries(node_index)%node%stmt_label)) return
        query%statement_label = arena%entries(node_index)%node%stmt_label
        query%has_statement_label = len(query%statement_label) > 0
    end subroutine copy_statement_label

    subroutine fill_goto_query(arena, node_index, node, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(goto_node), intent(in) :: node
        type(branch_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = BRANCH_GOTO
        call copy_text(node%label, query%target_label, query%has_target_label)
        call copy_text(node%label_list, query%target_labels, &
            query%has_target_labels)
        call copy_index(node%selector_index, query%selector_node_index, &
            query%has_selector)
        query%is_computed = query%has_target_labels .or. query%has_selector
        call collect_goto_targets(arena, node_index, query)
    end subroutine fill_goto_query

    subroutine collect_goto_targets(arena, source_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: source_index
        type(branch_statement_query_t), intent(inout) :: query
        character(len=:), allocatable :: remaining, label
        integer :: comma

        if (query%has_target_label) then
            call append_target(arena, source_index, query%target_label, query)
            return
        end if
        if (.not. query%has_target_labels) return
        remaining = query%target_labels
        do
            comma = index(remaining, ',')
            if (comma == 0) then
                label = trim(adjustl(remaining))
            else
                label = trim(adjustl(remaining(:comma - 1)))
            end if
            call append_target(arena, source_index, label, query)
            if (comma == 0) exit
            remaining = remaining(comma + 1:)
        end do
    end subroutine collect_goto_targets

    subroutine append_target(arena, source_index, label, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: source_index
        character(len=*), intent(in) :: label
        type(branch_statement_query_t), intent(inout) :: query
        type(branch_target_query_t) :: target

        target%label = label
        target%node_index = resolve_target_node(arena, source_index, label)
        target%has_node = target%node_index > 0
        query%targets = [query%targets, target]
    end subroutine append_target

    integer function resolve_target_node(arena, source_index, label) &
            result(target_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: source_index
        character(len=*), intent(in) :: label
        integer :: i, source_scope, matches

        target_index = 0
        matches = 0
        source_scope = nearest_scoping_unit(arena, source_index)
        if (source_scope <= 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. allocated(arena%entries(i)%node%stmt_label)) cycle
            if (trim(arena%entries(i)%node%stmt_label) /= trim(label)) cycle
            if (nearest_scoping_unit(arena, i) /= source_scope) cycle
            matches = matches + 1
            target_index = i
        end do
        if (matches == 1) return
        target_index = 0
    end function resolve_target_node

    integer function nearest_scoping_unit(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current_index, parent_index, steps

        scope_index = 0
        current_index = node_index
        do steps = 1, arena%size
            if (is_scoping_unit(arena, current_index)) then
                scope_index = current_index
                return
            end if
            parent_index = arena%entries(current_index)%parent_index
            if (parent_index <= 0) return
            if (.not. arena%has_node_at(parent_index)) return
            current_index = parent_index
        end do
    end function nearest_scoping_unit

    logical function is_scoping_unit(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_scoping_unit = .false.
        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            is_scoping_unit = .true.
            type is (function_def_node)
            is_scoping_unit = .true.
            type is (subroutine_def_node)
            is_scoping_unit = .true.
        end select
    end function is_scoping_unit

    subroutine fill_pause_query(node, query)
        type(pause_node), intent(in) :: node
        type(branch_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = BRANCH_PAUSE
        call copy_index(node%pause_code_index, query%code_node_index, &
            query%has_code)
        call copy_text(node%pause_message, query%message, query%has_message)
    end subroutine fill_pause_query

    subroutine copy_text(source, target, present_flag)
        character(len=:), allocatable, intent(in) :: source
        character(len=:), allocatable, intent(inout) :: target
        logical, intent(inout) :: present_flag

        if (.not. allocated(source)) return
        target = source
        present_flag = .true.
    end subroutine copy_text

    subroutine copy_index(source, target, present_flag)
        integer, intent(in) :: source
        integer, intent(inout) :: target
        logical, intent(inout) :: present_flag

        if (source <= 0) return
        target = source
        present_flag = .true.
    end subroutine copy_index

end module frontend_compiler_branch_queries
