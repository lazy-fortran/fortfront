module codegen_control_flow
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_control
    use type_system_unified
    use codegen_indent
    use codegen_grouped_body, only: generate_grouped_body
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private

    logical, parameter :: enable_single_line_if = .true.

    public :: generate_code_if
    public :: generate_code_do_loop
    public :: generate_code_do_while
    public :: generate_code_select_case
    public :: generate_code_select_type
    public :: generate_code_select_rank
    public :: generate_code_where
    public :: generate_code_forall
    public :: generate_code_associate
    public :: generate_code_block_construct

contains

    recursive subroutine append_do_loop_body_and_end(arena, body_indices, &
            indent_level, code, &
            end_keyword)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        integer, intent(in) :: indent_level
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in), optional :: end_keyword
        character(len=:), allocatable :: body_code, end_stmt

        if (present(end_keyword)) then
            end_stmt = end_keyword
        else
            end_stmt = "end do"
        end if

        if (allocated(body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
                code = code//repeat("    ", indent_level)//end_stmt
            else
                code = code//new_line('A')//repeat("    ", indent_level) &
                    //end_stmt
            end if
        else
            code = code//new_line('A')//repeat("    ", indent_level)//end_stmt
        end if
    end subroutine append_do_loop_body_and_end

    ! Generate code for if statements
    function generate_code_if(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code, single_line
        integer :: i, indent_level
        logical :: has_no_elseif, has_no_else

        indent_level = 0

        ! Generate condition
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ""
        end if

        ! Skip IF with empty condition (malformed node from parser bug like issue #1736)
        if (len_trim(cond_code) == 0) then
            code = ""
            return
        end if

        has_no_elseif = .not. allocated(node%elseif_blocks)
        if (.not. has_no_elseif) then
            has_no_elseif = size(node%elseif_blocks) == 0
        end if

        has_no_else = .not. allocated(node%else_body_indices)
        if (.not. has_no_else) then
            has_no_else = size(node%else_body_indices) == 0
        end if

        if (enable_single_line_if .and. has_no_elseif .and. has_no_else) then
            single_line = try_generate_single_line_if(arena, node, cond_code)
            if (allocated(single_line)) then
                if (len(single_line) > 0) then
                    code = single_line
                    return
                end if
            end if
        end if

        ! Generate if statement in block form
        code = "if ("//cond_code//") then"

        ! Generate then body
        if (allocated(node%then_body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, node%then_body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        ! Generate else if blocks
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                cond_code = generate_code_from_arena( &
                    arena, node%elseif_blocks(i)%condition_index)
                code = code//new_line('A')//repeat("    ", indent_level)// &
                    "else if ("//cond_code//") then"

                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    body_code = generate_grouped_body_internal( &
                        arena, node%elseif_blocks(i)%body_indices, &
                        indent_level + 1)
                    if (len(body_code) > 0) then
                        code = code//new_line('A')//body_code
                    end if
                end if
            end do
        end if

        ! Generate else block
        if (allocated(node%else_body_indices)) then
            code = code//new_line('A')//repeat("    ", indent_level)//"else"
            body_code = generate_grouped_body_internal( &
                arena, node%else_body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        ! Generate end if
        code = code//new_line('A')//repeat("    ", indent_level)//"end if"
    end function generate_code_if

    ! Try to generate single-line IF statement if conditions are met
    function try_generate_single_line_if(arena, node, cond_code) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        character(len=*), intent(in) :: cond_code
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        integer :: stmt_index

        if (.not. allocated(node%then_body_indices)) return
        if (size(node%then_body_indices) /= 1) return

        stmt_index = node%then_body_indices(1)
        if (.not. arena%has_node_at(stmt_index)) return

        if (trim(arena%entries(stmt_index)%node_type) == "print_statement") return

        select type (stmt_node => arena%entries(stmt_index)%node)
        class default
            if (stmt_node%line /= node%line) return

            stmt_code = generate_code_from_arena(arena, stmt_index)
            stmt_code = trim(adjustl(stmt_code))
            if (len_trim(stmt_code) == 0) return
            if (index(stmt_code, new_line('A')) /= 0) return

            code = "if ("//trim(adjustl(cond_code))//") "//stmt_code
        end select
    end function try_generate_single_line_if

    ! Generate code for do loops
    recursive function generate_code_do_loop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: var_code, start_code, end_code, step_code
        character(len=:), allocatable :: body_code, end_keyword, index_code
        integer :: indent_level

        indent_level = 0

        ! Generate loop variable
        if (allocated(node%var_name)) then
            var_code = trim(adjustl(node%var_name))
        else
            var_code = ""
        end if

        ! Generate loop bounds
        if (node%start_expr_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
            start_code = trim(adjustl(start_code))
        else
            start_code = ""
        end if

        if (node%end_expr_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
            end_code = trim(adjustl(end_code))
        else
            end_code = ""
        end if

        if (node%step_expr_index > 0) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
            step_code = trim(adjustl(step_code))
        else
            step_code = ""
        end if

        ! Generate do statement with optional label
        if (node%is_concurrent) then
            ! DO CONCURRENT syntax: do concurrent ([type-spec ::] var = start:end[:step])
            index_code = var_code
            if (allocated(node%type_spec) .and. len_trim(node%type_spec) > 0) then
                index_code = trim(node%type_spec)//" :: "//var_code
            end if
            if (allocated(node%label)) then
                code = trim(adjustl(node%label))//": do concurrent ("// &
                    index_code// &
                    " = "//start_code//":"//end_code
                end_keyword = "end do "//trim(adjustl(node%label))
            else
                code = "do concurrent ("//index_code//" = "//start_code//":"// &
                    end_code
                end_keyword = "end do"
            end if
            if (len(step_code) > 0) then
                code = code//":"//step_code
            end if
            code = code//")"
        else
            ! Regular DO loop syntax: do var = start, end [, step]
            if (allocated(node%label)) then
                code = trim(adjustl(node%label))//": do "//var_code//" = "// &
                    start_code//", "//end_code
                end_keyword = "end do "//trim(adjustl(node%label))
            else
                code = "do "//var_code//" = "//start_code//", "//end_code
                end_keyword = "end do"
            end if
            if (len(step_code) > 0) then
                code = code//", "//step_code
            end if
        end if

        ! Generate body
        call append_do_loop_body_and_end(arena, node%body_indices, indent_level, &
            code, end_keyword)
    end function generate_code_do_loop

    ! Generate code for do while loops
    recursive function generate_code_do_while(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_while_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code, end_keyword
        integer :: indent_level

        indent_level = 0

        ! Generate condition
        if (node%condition_index > 0) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ""
        end if

        ! Generate do while statement with optional label
        if (allocated(node%label)) then
            code = trim(adjustl(node%label))//": do while ("//cond_code//")"
            end_keyword = "end do "//trim(adjustl(node%label))
        else
            code = "do while ("//cond_code//")"
            end_keyword = "end do"
        end if

        ! Generate body
        call append_do_loop_body_and_end(arena, node%body_indices, indent_level, &
            code, end_keyword)
    end function generate_code_do_while

    ! Generate code for select case statements
    function generate_code_select_case(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, case_code, body_code
        character(len=:), allocatable :: lower_code, upper_code
        integer :: i, j, indent_level

        indent_level = 0

        ! Generate case expression
        if (node%selector_index > 0) then
            expr_code = generate_code_from_arena(arena, node%selector_index)
        else
            expr_code = ""
        end if

        ! Generate select case statement
        code = "select case ("//expr_code//")"

        ! Generate case blocks
        if (allocated(node%case_indices)) then
            do i = 1, size(node%case_indices)
                if (node%case_indices(i) > 0 .and. &
                    node%case_indices(i) <= arena%size) then
                    select type (case_node => arena%entries(node%case_indices(i))%node)
                        type is (case_block_node)
                        ! Generate case statement
                        code = code//new_line('A')//repeat("    ", indent_level)
                        code = code//"case ("

                        ! Generate case values
                        if (allocated(case_node%value_indices)) then
                            do j = 1, size(case_node%value_indices)
                                if (j > 1) code = code//", "

                                if (case_node%value_indices(j) > 0 .and. &
                                    case_node%value_indices(j) <= arena%size) then
                                    select type (value_node => arena%entries( &
                                            case_node%value_indices(j))%node)
                                        type is (case_range_node)
                                        lower_code = generate_code_from_arena( &
                                            arena, value_node%start_value)
                                        if (value_node%end_value > 0) then
                                            upper_code = generate_code_from_arena( &
                                                arena, value_node%end_value)
                                            case_code = trim(adjustl(lower_code)) &
                                                //":"// &
                                                trim(adjustl(upper_code))
                                        else
                                            case_code = &
                                                trim(adjustl(lower_code))//":"
                                        end if
                                    class default
                                        case_code = generate_code_from_arena( &
                                            arena, case_node%value_indices(j))
                                    end select
                                    code = code//case_code
                                end if
                            end do
                        end if

                        code = code//")"

                        ! Generate case body
                        if (allocated(case_node%body_indices)) then
                            body_code = generate_grouped_body_internal( &
                                arena, case_node%body_indices, &
                                indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code//new_line('A')//body_code
                            end if
                        end if
                    end select
                end if
            end do
        end if

        ! Handle default case if present
        if (node%default_index > 0 .and. node%default_index <= arena%size) then
            code = code//new_line('A')//repeat("    ", indent_level)// &
                "case default"

            if (arena%has_node_at(node%default_index)) then
                select type (default_node => arena%entries(node%default_index)%node)
                    type is (case_default_node)
                    if (allocated(default_node%body_indices)) then
                        body_code = generate_grouped_body_internal( &
                            arena, default_node%body_indices, indent_level + 1)
                        if (len(body_code) > 0) then
                            code = code//new_line('A')//body_code
                        end if
                    end if
                class default
                    ! If the default entry is not a case_default_node, fall back to
                    ! direct generation.
                    body_code = generate_code_from_arena(arena, node%default_index)
                    if (len(body_code) > 0) then
                        code = code//new_line('A')//body_code
                    end if
                end select
            end if
        end if

        ! Generate end select
        code = code//new_line('A')//repeat("    ", indent_level)//"end select"
    end function generate_code_select_case

    ! Generate code for select type statements
    function generate_code_select_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, type_name_code, body_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate selector expression
        if (node%selector_index > 0) then
            expr_code = generate_code_from_arena(arena, node%selector_index)
        else
            expr_code = ""
        end if

        ! Generate select type statement
        code = "select type ("//expr_code//")"

        ! Generate type guard blocks
        if (allocated(node%guard_indices)) then
            do i = 1, size(node%guard_indices)
                if (node%guard_indices(i) > 0 .and. &
                    node%guard_indices(i) <= arena%size) then
                    select type (guard_node => &
                            arena%entries(node%guard_indices(i))%node)
                        type is (type_guard_block_node)
                        ! Generate type guard statement
                        code = code//new_line('A')//repeat("    ", indent_level)
                        if (guard_node%guard_type == "type_is") then
                            code = code//"type is"
                        else if (guard_node%guard_type == "class_is") then
                            code = code//"class is"
                        end if

                        ! Generate type name
                        if (guard_node%type_name_index > 0) then
                            type_name_code = generate_code_from_arena( &
                                arena, guard_node%type_name_index)
                            code = code//" ("//type_name_code//")"
                        end if

                        ! Generate guard body
                        if (allocated(guard_node%body_indices)) then
                            body_code = generate_grouped_body_internal( &
                                arena, guard_node%body_indices, &
                                indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code//new_line('A')//body_code
                            end if
                        end if
                    end select
                end if
            end do
        end if

        ! Handle default guard if present
        if (node%default_index > 0 .and. node%default_index <= arena%size) then
            select type (default_node => arena%entries(node%default_index)%node)
                type is (type_guard_block_node)
                code = code//new_line('A')//repeat("    ", indent_level)// &
                    "class default"

                if (allocated(default_node%body_indices)) then
                    body_code = generate_grouped_body_internal( &
                        arena, default_node%body_indices, indent_level + 1)
                    if (len(body_code) > 0) then
                        code = code//new_line('A')//body_code
                    end if
                end if
            end select
        end if

        ! Generate end select
        code = code//new_line('A')//repeat("    ", indent_level)//"end select"
    end function generate_code_select_type

    ! Generate code for SELECT RANK constructs
    function generate_code_select_rank(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_rank_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, body_code
        integer :: i, indent_level
        character(len=20) :: rank_str

        indent_level = 0

        ! Generate selector expression
        if (node%selector_index > 0) then
            expr_code = generate_code_from_arena(arena, node%selector_index)
        else
            expr_code = ""
        end if

        ! Generate select rank statement
        code = "select rank ("//expr_code//")"

        ! Generate rank blocks
        if (allocated(node%rank_indices)) then
            do i = 1, size(node%rank_indices)
                if (node%rank_indices(i) > 0 .and. &
                    node%rank_indices(i) <= arena%size) then
                    select type (rank_node => &
                            arena%entries(node%rank_indices(i))%node)
                        type is (rank_block_node)
                        ! Generate rank statement
                        code = code//new_line('A')//repeat("    ", indent_level)

                        if (rank_node%rank_value == -2) then
                            ! RANK (*)
                            code = code//"rank (*)"
                        else if (rank_node%rank_value >= 0) then
                            ! RANK (n)
                            write (rank_str, '(I0)') rank_node%rank_value
                            code = code//"rank ("//trim(rank_str)//")"
                        end if

                        ! Generate rank body
                        if (allocated(rank_node%body_indices)) then
                            body_code = generate_grouped_body_internal( &
                                arena, rank_node%body_indices, &
                                indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code//new_line('A')//body_code
                            end if
                        end if
                    end select
                end if
            end do
        end if

        ! Handle default rank if present
        if (node%default_index > 0 .and. node%default_index <= arena%size) then
            select type (default_node => arena%entries(node%default_index)%node)
                type is (rank_block_node)
                code = code//new_line('A')//repeat("    ", indent_level)// &
                    "rank default"

                if (allocated(default_node%body_indices)) then
                    body_code = generate_grouped_body_internal( &
                        arena, default_node%body_indices, indent_level + 1)
                    if (len(body_code) > 0) then
                        code = code//new_line('A')//body_code
                    end if
                end if
            end select
        end if

        ! Generate end select
        code = code//new_line('A')//repeat("    ", indent_level)//"end select"
    end function generate_code_select_rank

    ! Generate code for where constructs
    function generate_code_where(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: mask_code, body_code
        integer :: i

        ! Generate mask expression
        if (node%mask_expr_index > 0) then
            mask_code = generate_code_from_arena(arena, node%mask_expr_index)
        else
            mask_code = ".true."
        end if

        ! WHERE header
        code = "where ("//mask_code//")"

        ! WHERE body
        if (allocated(node%where_body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, node%where_body_indices, 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        ! ELSEWHERE clauses (including final ELSEWHERE without mask)
        if (allocated(node%elsewhere_clauses)) then
            do i = 1, size(node%elsewhere_clauses)
                if (node%elsewhere_clauses(i)%mask_index > 0) then
                    mask_code = generate_code_from_arena( &
                        arena, node%elsewhere_clauses(i)%mask_index)
                    code = code//new_line('A')//"elsewhere ("//mask_code//")"
                else
                    code = code//new_line('A')//"elsewhere"
                end if

                if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                    body_code = generate_grouped_body_internal( &
                        arena, node%elsewhere_clauses(i)%body_indices, 1)
                    if (len(body_code) > 0) then
                        code = code//new_line('A')//body_code
                    end if
                end if
            end do
        end if

        ! END WHERE
        code = code//new_line('A')//"end where"
    end function generate_code_where

    ! Generate code for forall constructs
    function generate_code_forall(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(forall_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header, part
        character(len=:), allocatable :: body_code
        integer :: i

        header = "forall ("
        if (node%num_indices > 0 .and. allocated(node%index_names) .and. &
            allocated(node%lower_bound_indices) .and. &
            allocated(node%upper_bound_indices) .and. &
            allocated(node%stride_indices)) then
            do i = 1, node%num_indices
                if (i > 1) header = header//", "

                part = ""
                part = trim(node%index_names(i))//" = "

                if (node%lower_bound_indices(i) > 0) then
                    part = part//generate_code_from_arena( &
                        arena, node%lower_bound_indices(i))
                end if
                part = part//":"
                if (node%upper_bound_indices(i) > 0) then
                    part = part//generate_code_from_arena( &
                        arena, node%upper_bound_indices(i))
                end if
                if (node%stride_indices(i) > 0) then
                    part = part//":"//generate_code_from_arena( &
                        arena, node%stride_indices(i))
                end if

                header = header//part
            end do
        end if

        if (node%has_mask .and. node%mask_expr_index > 0) then
            header = header//", "//generate_code_from_arena( &
                arena, node%mask_expr_index)
        end if
        header = header//")"

        code = header

        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, node%body_indices, 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        code = code//new_line('A')//"end forall"
    end function generate_code_forall

    ! Generate code for associate constructs
    function generate_code_associate(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(associate_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: assoc_code, body_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate associate statement
        code = "associate ("

        ! Generate associations
        if (allocated(node%associations)) then
            do i = 1, size(node%associations)
                if (i > 1) code = code//", "

                if (allocated(node%associations(i)%name)) then
                    code = code//node%associations(i)%name//" => "

                    if (node%associations(i)%expr_index > 0) then
                        assoc_code = generate_code_from_arena( &
                            arena, node%associations(i)%expr_index)
                        code = code//assoc_code
                    end if
                end if
            end do
        end if

        code = code//")"

        ! Generate body
        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, node%body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        ! Generate end associate
        code = code//new_line('A')//repeat("    ", indent_level)
        code = code//"end associate"
    end function generate_code_associate

    function generate_code_block_construct(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(block_construct_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: indent_level

        indent_level = 0
        code = "block"

        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal( &
                arena, node%body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code//new_line('A')//body_code
            end if
        end if

        code = code//new_line('A')//repeat("    ", indent_level)
        code = code//"end block"
    end function generate_code_block_construct

    ! Internal function to generate grouped body
    recursive function generate_grouped_body_internal(arena, body_indices, indent) &
            result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code

        ! Delegate to shared body generation utility
        code = generate_grouped_body(arena, body_indices, indent)
    end function generate_grouped_body_internal

end module codegen_control_flow
