program test_issue_1619_nested_types_complex
    ! Comprehensive test coverage for nested derived types (issue #1619)
    !
    ! This test verifies support for complex nested type patterns beyond basic cases.
    ! Tests constructor interface pattern and module integration.
    !
    ! Known limitations (to be addressed in separate issues):
    ! - Nested type components are sometimes extracted outside type definitions
    ! - Allocatable components inside nested types are dropped
    ! - Deep nesting (3+ levels) has structural issues

    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    integer :: failures

    failures = 0
    failures = failures + test_constructor_interface_in_module()

    print *, ""
    if (failures == 0) then
        print *, "All issue 1619 tests passed."
    else
        print *, "Some tests failed. Total failures:", failures
        error stop 1
    end if

contains

    function test_constructor_interface_in_module() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Constructor interface pattern in module ==="

        call prepare_constructor_interface_input(input_code)
        print *, "Input:"
        print *, input_code

        call run_constructor_interface_pipeline(input_code, output_code, status)
        if (status /= 0) return

        call verify_constructor_interface_output(output_code, status)
        if (status /= 0) return

        print *, "PASS: Constructor interface pattern with nested types"
    end function

    subroutine prepare_constructor_interface_input(code)
        character(:), allocatable, intent(out) :: code

        code = &
            "module test_mod" // new_line('A') // &
            "implicit none" // new_line('A') // &
            new_line('A') // &
            "type :: t_inner" // new_line('A') // &
            "   integer :: value" // new_line('A') // &
            "end type t_inner" // new_line('A') // &
            new_line('A') // &
            "type :: t_outer" // new_line('A') // &
            "   integer :: id" // new_line('A') // &
            "   type(t_inner) :: inner" // new_line('A') // &
            "end type t_outer" // new_line('A') // &
            new_line('A') // &
            "interface t_outer" // new_line('A') // &
            "   module procedure new_outer" // new_line('A') // &
            "end interface t_outer" // new_line('A') // &
            new_line('A') // &
            "contains" // new_line('A') // &
            new_line('A') // &
            "function new_outer(id, inner) result(obj)" // new_line('A') // &
            "   integer, intent(in) :: id" // new_line('A') // &
            "   type(t_inner), intent(in) :: inner" // new_line('A') // &
            "   type(t_outer) :: obj" // new_line('A') // &
            "   obj%id = id" // new_line('A') // &
            "   obj%inner = inner" // new_line('A') // &
            "end function new_outer" // new_line('A') // &
            new_line('A') // &
            "end module test_mod"
    end subroutine

    subroutine run_constructor_interface_pipeline(input_code, output_code, status)
        character(len=*), intent(in) :: input_code
        character(:), allocatable, intent(out) :: output_code
        integer, intent(inout) :: status
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "FAIL: Lexer error:", trim(error_msg)
                status = 1
                return
            end if
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "FAIL: Parser error:", trim(error_msg)
                status = 1
                return
            end if
        end if

        if (prog_index <= 0) then
            print *, "FAIL: Parsing failed"
            status = 1
            return
        end if

        call emit_fortran(arena, prog_index, output_code)
        print *, "Output:"
        print *, output_code
    end subroutine

    subroutine verify_constructor_interface_output(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: inner_idx
        integer :: outer_idx
        integer :: outer_end_idx
        integer :: interface_idx
        integer :: procedure_idx
        integer :: function_idx

        outer_idx = index(output_code, "type :: t_outer")
        inner_idx = index(output_code, "type(t_inner) :: inner")
        interface_idx = index(output_code, "interface t_outer")
        procedure_idx = index(output_code, "module procedure new_outer")
        function_idx = index(output_code, "function new_outer")

        if (outer_idx == 0 .or. inner_idx == 0 .or. interface_idx == 0 .or. &
            procedure_idx == 0 .or. function_idx == 0 .or. &
            index(output_code, "type :: t_inner") == 0) then
            print *, "FAIL: Missing essential structure"
            status = 1
            return
        end if

        outer_end_idx = index(output_code, "end type t_outer")
        if (outer_end_idx == 0) then
            print *, "FAIL: Missing t_outer terminator"
            status = 1
            return
        end if

        if (inner_idx <= outer_idx .or. inner_idx >= outer_end_idx) then
            print *, "FAIL: Nested component not inside type definition"
            status = 1
        end if
    end subroutine

end program test_issue_1619_nested_types_complex
