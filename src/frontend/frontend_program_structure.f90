module frontend_program_structure
    ! Program structure creation and multi-unit handling
    ! Handles creating final program structures from parsed statements

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: blank_line_node, comment_node, directive_node, &
                              end_statement_node, implicit_statement_node, &
                              interface_block_node
    use ast_factory, only: push_program, push_multi_unit_container

    implicit none
    private

    ! Public program structure interface
    public :: create_final_program_structure, handle_multiple_program_units
    public :: should_include_program_unit, is_empty_main_program

contains

    ! Create final program structure from parsed statements
    subroutine create_final_program_structure(arena, body_indices, stmt_count, &
                                              prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        integer, intent(in) :: stmt_count
        integer, intent(out) :: prog_index

        character(len=:), allocatable :: prog_name

        if (size(body_indices) == 0) then
            ! Empty program
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
        else if (stmt_count == 1) then
            ! Single statement program
            prog_name = "main"
            prog_index = push_program(arena, prog_name, body_indices, 1, 1)
        else
            ! Multi-statement program
            prog_name = "main"
            prog_index = push_program(arena, prog_name, body_indices, 1, 1)
        end if
    end subroutine create_final_program_structure

    ! Handle multiple program units
    subroutine handle_multiple_program_units(arena, body_indices, &
                                             prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        integer, allocatable :: valid_units(:)
        integer :: i, valid_count

        error_msg = ""
        valid_count = 0
        allocate (valid_units(size(body_indices)))

        ! Filter out empty or invalid units
        do i = 1, size(body_indices)
            if (should_include_program_unit(arena, body_indices(i))) then
                valid_count = valid_count + 1
                valid_units(valid_count) = body_indices(i)
            end if
        end do

        if (valid_count == 0) then
            ! No valid units - create empty main program
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
        else if (valid_count == 1) then
            ! Single unit - check if it's already a program node
            if (allocated(arena%entries(valid_units(1))%node)) then
                select type (node => arena%entries(valid_units(1))%node)
                type is (program_node)
                    ! Already a program node
                    prog_index = valid_units(1)
                type is (interface_block_node)
                    prog_index = push_multi_unit_container(arena, &
                                                           valid_units(1:1), 1, 1)
                class default
                    ! Wrap in program node for consistent API
                    prog_index = push_program(arena, "main", valid_units(1:1), 1, 1)
                end select
            else
                ! Safety fallback
                prog_index = push_program(arena, "main", [integer ::], 1, 1)
            end if
        else
            ! Multiple units - create container
            prog_index = push_multi_unit_container(arena, &
                                                   valid_units(1:valid_count), 1, 1)
        end if

        deallocate (valid_units)
    end subroutine handle_multiple_program_units

    ! Check if program unit should be included
    function should_include_program_unit(arena, unit_index) result(should_include)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical :: should_include

        should_include = .true.

        if (unit_index <= 0 .or. unit_index > size(arena%entries)) then
            should_include = .false.
            return
        end if

        ! Check for empty main programs
        if (is_empty_main_program(arena%entries(unit_index)%node, arena)) then
            should_include = .false.
        end if
    end function should_include_program_unit

    ! Check if node is an empty main program
    function is_empty_main_program(node, arena) result(is_empty)
        class(*), intent(in) :: node
        type(ast_arena_t), intent(in) :: arena
        logical :: is_empty
        logical :: has_meaningful
        integer :: i
        integer :: idx

        is_empty = .false.

        select type (prog_node => node)
        type is (program_node)
            if (prog_node%name == "main" .or. &
                prog_node%name == "__IMPLICIT_MAIN__") then
                if (.not. allocated(prog_node%body_indices) .or. &
                    size(prog_node%body_indices) == 0) then
                    is_empty = .true.
                    return
                end if

                has_meaningful = .false.
                do i = 1, size(prog_node%body_indices)
                    idx = prog_node%body_indices(i)
                    if (.not. arena%has_node_at(idx)) cycle
                    select type (child => arena%entries(idx)%node)
                    type is (comment_node)
                        cycle
                    type is (blank_line_node)
                        cycle
                    type is (directive_node)
                        cycle
                    type is (implicit_statement_node)
                        if (.not. child%is_none) then
                            has_meaningful = .true.
                        end if
                    type is (end_statement_node)
                        cycle
                    class default
                        has_meaningful = .true.
                    end select
                    if (has_meaningful) exit
                end do

                if (.not. has_meaningful) is_empty = .true.
            end if
        end select
    end function is_empty_main_program

end module frontend_program_structure
