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
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        status = 0

        input_code = &
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

        print *, ""
        print *, "=== Test: Constructor interface pattern in module ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            status = 1
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            status = 1
            return
        end if

        if (prog_index > 0) then
            call emit_fortran(arena, prog_index, output_code)
            print *, "Output:"
            print *, output_code

            ! Verify key structures are preserved
            if (index(output_code, "type :: t_inner") == 0 .or. &
                index(output_code, "type :: t_outer") == 0 .or. &
                index(output_code, "interface t_outer") == 0 .or. &
                index(output_code, "module procedure new_outer") == 0 .or. &
                index(output_code, "function new_outer") == 0 .or. &
                index(output_code, "type(t_inner) :: inner") == 0) then
                print *, "FAIL: Missing essential structure"
                status = 1
                return
            end if

            ! Verify nested component is inside t_outer type definition
            if (index(output_code, "type(t_inner) :: inner") < &
                index(output_code, "type :: t_outer") .or. &
                index(output_code, "type(t_inner) :: inner") > &
                index(output_code, "end type t_outer")) then
                print *, "FAIL: Nested component not inside type definition"
                status = 1
                return
            end if

            print *, "PASS: Constructor interface pattern with nested types"
        else
            print *, "FAIL: Parsing failed"
            status = 1
        end if
    end function

end program test_issue_1619_nested_types_complex
