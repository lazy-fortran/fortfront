program test_location_validation
    ! Test AST location validation pass
    ! Verifies that parser populates source locations correctly

    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: assignment_node
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t, tokenize_core
    use frontend_location_validation, only: validate_ast_locations
    implicit none
    integer :: status

    status = 0

    call test_simple_assignment_has_location()
    call test_function_has_location()
    call test_reports_invalid_coordinates()

    if (status /= 0) then
        write (error_unit, '(A)') "FAIL: test_location_validation"
        stop 1
    end if

contains

    subroutine test_simple_assignment_has_location()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        character(len=256) :: error_msg
        integer :: prog_index, violations

        source = "x = 42"
        error_msg = ""

        arena = create_ast_arena()
        call tokenize_core(source, tokens)
        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A,A)') "Parse error: ", trim(error_msg)
            status = 1
            return
        end if

        call validate_ast_locations(arena, strict_mode=.false., &
            violations_count=violations)

        ! Parser should populate all node locations for this input
        if (violations /= 0) then
            write (error_unit, '(A,I0)') &
                "FAIL: Expected zero violations, saw ", violations
            status = 1
        end if
    end subroutine test_simple_assignment_has_location

    subroutine test_function_has_location()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        character(len=256) :: error_msg
        integer :: prog_index, violations

        source = "function add(a, b)" // new_line('a') // &
            "    result = a + b" // new_line('a') // &
            "end function"
        error_msg = ""

        arena = create_ast_arena()
        call tokenize_core(source, tokens)
        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A,A)') "Parse error: ", trim(error_msg)
            status = 1
            return
        end if

        call validate_ast_locations(arena, strict_mode=.false., &
            violations_count=violations)

        if (violations /= 0) then
            write (error_unit, '(A,I0)') &
                "FAIL: Expected zero violations, saw ", violations
            status = 1
        end if
    end subroutine test_function_has_location

    subroutine test_reports_invalid_coordinates()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        character(len=256) :: error_msg
        integer :: prog_index, violations

        source = "y = 5"
        error_msg = ""

        arena = create_ast_arena()
        call tokenize_core(source, tokens)
        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A,A)') "Parse error: ", trim(error_msg)
            status = 1
            return
        end if

        call invalidate_first_assignment(arena)

        call validate_ast_locations(arena, strict_mode=.false., &
            violations_count=violations)

        if (violations /= 1) then
            write (error_unit, '(A,I0)') &
                "FAIL: Expected one violation after tampering, saw ", &
                violations
            status = 1
        end if
    end subroutine test_reports_invalid_coordinates

    subroutine invalidate_first_assignment(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i
        logical :: updated

        updated = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (assign_node => arena%entries(i)%node)
                type is (assignment_node)
                assign_node%line = 0
                assign_node%column = 0
                updated = .true.
                exit
            end select
        end do

        if (.not. updated) then
            write (error_unit, '(A)') &
                "FAIL: Unable to locate assignment node for tampering"
            status = 1
        end if
    end subroutine invalidate_first_assignment

end program test_location_validation
