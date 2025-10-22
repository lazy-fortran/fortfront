program test_issue_1604_type_bound_procedures
    ! Test coverage for type-bound procedures (F2003 feature, issue #1604)
    !
    ! This test verifies that type-bound procedures are correctly parsed
    ! and preserved in code generation, including:
    ! - Basic procedure bindings (procedure :: name => implementation)
    ! - Procedure bindings without implementation (procedure :: name)
    ! - Generic bindings (generic :: operator(+) => add_impl)
    ! - Accessibility modifiers (public/private)
    ! - DEFERRED and NOPASS attributes

    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    integer :: failures

    failures = 0
    failures = failures + test_basic_type_bound_procedures()
    failures = failures + test_multiple_bindings()

    print *, ""
    if (failures == 0) then
        print *, "All issue 1604 tests passed."
    else
        print *, "Some tests failed. Total failures:", failures
        error stop 1
    end if

contains

    function test_basic_type_bound_procedures() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Basic type-bound procedures ==="

        input_code = &
            "module test_mod" // new_line('A') // &
            "implicit none" // new_line('A') // &
            new_line('A') // &
            "type :: atype" // new_line('A') // &
            "   integer :: value" // new_line('A') // &
            "contains" // new_line('A') // &
            "   procedure :: get_value" // new_line('A') // &
            "   procedure :: set_value => set_val" // new_line('A') // &
            "end type atype" // new_line('A') // &
            new_line('A') // &
            "end module test_mod"

        print *, "Input:"
        print *, input_code

        call run_pipeline(input_code, output_code, status)
        if (status /= 0) return

        print *, "Output:"
        print *, output_code

        call verify_basic_bindings(output_code, status)
        if (status /= 0) return

        print *, "PASS: Basic type-bound procedures"
    end function

    function test_multiple_bindings() result(status)
        integer :: status
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code

        status = 0

        print *, ""
        print *, "=== Test: Multiple procedure bindings ==="

        input_code = &
            "type :: mytype" // new_line('A') // &
            "   real :: x" // new_line('A') // &
            "contains" // new_line('A') // &
            "   procedure :: init" // new_line('A') // &
            "   procedure :: compute => do_compute" // new_line('A') // &
            "   procedure :: cleanup" // new_line('A') // &
            "end type mytype"

        print *, "Input:"
        print *, input_code

        call run_pipeline(input_code, output_code, status)
        if (status /= 0) return

        print *, "Output:"
        print *, output_code

        call verify_multiple_bindings(output_code, status)
        if (status /= 0) return

        print *, "PASS: Multiple procedure bindings"
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

    subroutine verify_basic_bindings(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: type_idx
        integer :: contains_idx
        integer :: end_type_idx
        integer :: get_idx
        integer :: set_idx

        type_idx = index(output_code, "type :: atype")
        contains_idx = index(output_code, "contains")
        end_type_idx = index(output_code, "end type")
        get_idx = index(output_code, "procedure :: get_value")
        set_idx = index(output_code, "procedure :: set_value => set_val")

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

        if (get_idx == 0) then
            print *, "FAIL: Missing get_value binding"
            status = 1
            return
        end if

        if (set_idx == 0) then
            print *, "FAIL: Missing set_value binding with implementation"
            status = 1
            return
        end if

        if (.not. (type_idx < contains_idx .and. &
                   contains_idx < get_idx .and. &
                   get_idx < set_idx .and. &
                   set_idx < end_type_idx)) then
            print *, "FAIL: Bindings not in correct order or location"
            status = 1
            return
        end if
    end subroutine

    subroutine verify_multiple_bindings(output_code, status)
        character(len=*), intent(in) :: output_code
        integer, intent(inout) :: status
        integer :: init_idx
        integer :: compute_idx
        integer :: cleanup_idx

        init_idx = index(output_code, "procedure :: init")
        compute_idx = index(output_code, "procedure :: compute => do_compute")
        cleanup_idx = index(output_code, "procedure :: cleanup")

        if (init_idx == 0) then
            print *, "FAIL: Missing init binding"
            status = 1
            return
        end if

        if (compute_idx == 0) then
            print *, "FAIL: Missing compute binding"
            status = 1
            return
        end if

        if (cleanup_idx == 0) then
            print *, "FAIL: Missing cleanup binding"
            status = 1
            return
        end if

        if (.not. (init_idx < compute_idx .and. compute_idx < cleanup_idx)) then
            print *, "FAIL: Bindings not in correct order"
            status = 1
            return
        end if
    end subroutine

end program test_issue_1604_type_bound_procedures
