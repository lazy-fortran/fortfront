program test_issue_1607_final_procedures
    ! Test coverage for final procedures (F2003 feature, issue #1607)
    !
    ! This test verifies that final procedures are correctly parsed
    ! and preserved in code generation. Final procedures provide
    ! automatic cleanup when objects go out of scope (destructors).

    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    integer :: failures

    failures = 0
    failures = failures + test_basic_final()
    failures = failures + test_multiple_finals()
    failures = failures + test_final_in_module()

    print *, ""
    if (failures == 0) then
        print *, "All issue 1607 tests passed."
    else
        print *, "Some tests failed. Total failures:", failures
        error stop 1
    end if

contains

    function test_basic_final() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Basic final procedure ==="

        input_code = &
            "type :: myclass_t" // new_line('A') // &
            "   integer :: value" // new_line('A') // &
            "contains" // new_line('A') // &
            "   final :: myclass_destroy" // new_line('A') // &
            "end type myclass_t"

        print *, "Input:"
        print *, input_code

        call run_pipeline(input_code, output_code, status)
        if (status /= 0) return

        print *, "Output:"
        print *, output_code

        call verify_basic_final(output_code, status)
        if (status /= 0) return

        print *, "PASS: Basic final procedure"
    end function

    function test_multiple_finals() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Multiple final procedures ==="

        input_code = &
            "type :: resource_t" // new_line('A') // &
            "   real :: x" // new_line('A') // &
            "contains" // new_line('A') // &
            "   procedure :: init" // new_line('A') // &
            "   final :: cleanup_scalar" // new_line('A') // &
            "   final :: cleanup_array" // new_line('A') // &
            "end type resource_t"

        print *, "Input:"
        print *, input_code

        call run_pipeline(input_code, output_code, status)
        if (status /= 0) return

        print *, "Output:"
        print *, output_code

        call verify_multiple_finals(output_code, status)
        if (status /= 0) return

        print *, "PASS: Multiple final procedures"
    end function

    function test_final_in_module() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Final procedure in module ==="

        input_code = &
            "module test_mod" // new_line('A') // &
            "implicit none" // new_line('A') // &
            new_line('A') // &
            "type :: myclass_t" // new_line('A') // &
            "   integer :: value" // new_line('A') // &
            "contains" // new_line('A') // &
            "   procedure :: get_val => myclass_get_val" // new_line('A') // &
            "   final :: myclass_destroy" // new_line('A') // &
            "end type myclass_t" // new_line('A') // &
            new_line('A') // &
            "end module test_mod"

        print *, "Input:"
        print *, input_code

        call run_pipeline(input_code, output_code, status)
        if (status /= 0) return

        print *, "Output:"
        print *, output_code

        call verify_final_in_module(output_code, status)
        if (status /= 0) return

        print *, "PASS: Final procedure in module"
    end function

    subroutine run_pipeline(input_code, output_code, status)
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
    end subroutine

    subroutine verify_basic_final(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: type_idx
        integer :: contains_idx
        integer :: final_idx
        integer :: end_type_idx

        type_idx = index(output_code, "type :: myclass_t")
        contains_idx = index(output_code, "contains")
        final_idx = index(output_code, "final :: myclass_destroy")
        end_type_idx = index(output_code, "end type")

        if (type_idx == 0) then
            print *, "FAIL: Missing type definition"
            status = 1
            return
        end if

        if (contains_idx == 0) then
            print *, "FAIL: Missing CONTAINS section"
            status = 1
            return
        end if

        if (final_idx == 0) then
            print *, "FAIL: Missing final procedure"
            status = 1
            return
        end if

        if (.not. (type_idx < contains_idx .and. &
                   contains_idx < final_idx .and. &
                   final_idx < end_type_idx)) then
            print *, "FAIL: Final not in correct location"
            status = 1
            return
        end if
    end subroutine

    subroutine verify_multiple_finals(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: init_idx
        integer :: scalar_idx
        integer :: array_idx

        init_idx = index(output_code, "procedure :: init")
        scalar_idx = index(output_code, "final :: cleanup_scalar")
        array_idx = index(output_code, "final :: cleanup_array")

        if (init_idx == 0) then
            print *, "FAIL: Missing procedure binding"
            status = 1
            return
        end if

        if (scalar_idx == 0) then
            print *, "FAIL: Missing final :: cleanup_scalar"
            status = 1
            return
        end if

        if (array_idx == 0) then
            print *, "FAIL: Missing final :: cleanup_array"
            status = 1
            return
        end if

        if (.not. (init_idx < scalar_idx .and. scalar_idx < array_idx)) then
            print *, "FAIL: Bindings not in correct order"
            status = 1
            return
        end if
    end subroutine

    subroutine verify_final_in_module(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: module_idx
        integer :: type_idx
        integer :: get_idx
        integer :: final_idx
        integer :: end_module_idx

        module_idx = index(output_code, "module test_mod")
        type_idx = index(output_code, "type :: myclass_t")
        get_idx = index(output_code, "procedure :: get_val => myclass_get_val")
        final_idx = index(output_code, "final :: myclass_destroy")
        end_module_idx = index(output_code, "end module")

        if (module_idx == 0) then
            print *, "FAIL: Missing module"
            status = 1
            return
        end if

        if (type_idx == 0) then
            print *, "FAIL: Missing type definition"
            status = 1
            return
        end if

        if (get_idx == 0) then
            print *, "FAIL: Missing procedure binding"
            status = 1
            return
        end if

        if (final_idx == 0) then
            print *, "FAIL: Missing final procedure"
            status = 1
            return
        end if

        if (.not. (module_idx < type_idx .and. &
                   type_idx < get_idx .and. &
                   get_idx < final_idx .and. &
                   final_idx < end_module_idx)) then
            print *, "FAIL: Elements not in correct order"
            status = 1
            return
        end if
    end subroutine

end program test_issue_1607_final_procedures
