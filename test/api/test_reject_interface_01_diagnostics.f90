program test_reject_interface_01_diagnostics
    ! Rejection coverage for explicit-interface declaration rules (issue #2883).
    !
    ! Rule under test: F2018 C1414 (F2003 C1204). A module-procedure-stmt may
    ! appear only in a generic interface block. An ABSTRACT INTERFACE block is
    ! never generic, so MODULE PROCEDURE inside one is invalid.
    ! Represented by gfortran.dg/interface_abstract_3.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject invalid interface declarations (issue #2883) ==='

    call test_module_procedure_in_abstract_interface_rejected(all_tests_passed)
    call test_module_procedure_double_colon_rejected(all_tests_passed)
    call test_module_procedure_in_generic_interface_accepted(all_tests_passed)
    call test_abstract_interface_with_subroutine_body_accepted(all_tests_passed)
    call test_abstract_interface_with_function_body_accepted(all_tests_passed)
    call test_external_statement_for_interface_name_rejected(all_tests_passed)
    call test_intrinsic_statement_for_interface_name_rejected(all_tests_passed)
    call test_attribute_outside_interface_body_rejected(all_tests_passed)
    call test_contained_procedure_with_interface_rejected(all_tests_passed)
    call test_intrinsic_as_module_procedure_rejected(all_tests_passed)
    call test_assumed_length_module_function_rejected(all_tests_passed)
    call test_call_without_required_interface_rejected(all_tests_passed)
    call test_external_statement_without_interface_accepted(all_tests_passed)
    call test_attribute_inside_interface_body_accepted(all_tests_passed)
    call test_separate_module_procedure_accepted(all_tests_passed)
    call test_assumed_length_external_function_accepted(all_tests_passed)
    call test_call_with_interface_block_accepted(all_tests_passed)
    call test_call_with_use_associated_interface_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All interface declaration rejection tests passed'
        stop 0
    else
        print *, 'Some interface declaration rejection tests failed'
        stop 1
    end if

contains

    subroutine test_module_procedure_in_abstract_interface_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE in ABSTRACT INTERFACE (rejected)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'module procedure p'//new_line('a')// &
            'end interface'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine p()'//new_line('a')// &
            'end subroutine p'//new_line('a')// &
            'end module m'
        call expect_frontend_error(source, 'ABSTRACT INTERFACE', passed)
    end subroutine test_module_procedure_in_abstract_interface_rejected

    subroutine test_module_procedure_double_colon_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE :: in ABSTRACT INTERFACE (rejected)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'module procedure :: p, q'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_error(source, 'MODULE PROCEDURE', passed)
    end subroutine test_module_procedure_double_colon_rejected

    subroutine test_module_procedure_in_generic_interface_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE in a generic interface (accepted)...'
        source = 'module m'//new_line('a')// &
            'interface gen'//new_line('a')// &
            'module procedure p'//new_line('a')// &
            'end interface gen'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine p()'//new_line('a')// &
            'end subroutine p'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_module_procedure_in_generic_interface_accepted

    subroutine test_abstract_interface_with_subroutine_body_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ABSTRACT INTERFACE with a subroutine body (accepted)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'subroutine handler(x)'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'end subroutine handler'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_abstract_interface_with_subroutine_body_accepted

    subroutine test_abstract_interface_with_function_body_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ABSTRACT INTERFACE with a function body (accepted)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'pure function scorer(x) result(v)'//new_line('a')// &
            'real, intent(in) :: x'//new_line('a')// &
            'real :: v'//new_line('a')// &
            'end function scorer'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_abstract_interface_with_function_body_accepted

    ! interface_23: an interface body already makes the name external.
    subroutine test_external_statement_for_interface_name_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing EXTERNAL for an interface-declared name (rejected)...'
        source = 'module a'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine foo'//new_line('a')// &
            'end subroutine'//new_line('a')// &
            'end interface'//new_line('a')// &
            'external foo'//new_line('a')// &
            'end module a'
        call expect_frontend_error(source, 'Duplicate EXTERNAL attribute', passed)
    end subroutine test_external_statement_for_interface_name_rejected

    ! interface_23: INTRINSIC conflicts with the implied EXTERNAL attribute.
    subroutine test_intrinsic_statement_for_interface_name_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing INTRINSIC for an interface-declared name (rejected)...'
        source = 'module b'//new_line('a')// &
            'interface'//new_line('a')// &
            'function sin(x)'//new_line('a')// &
            'real :: sin, x'//new_line('a')// &
            'end function'//new_line('a')// &
            'end interface'//new_line('a')// &
            'intrinsic sin'//new_line('a')// &
            'end module b'
        call expect_frontend_error(source, 'INTRINSIC attribute', passed)
    end subroutine test_intrinsic_statement_for_interface_name_rejected

    ! interface_24: attributes belong inside the interface body.
    subroutine test_attribute_outside_interface_body_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing attribute outside an INTERFACE body (rejected)...'
        source = 'module m1'//new_line('a')// &
            'interface'//new_line('a')// &
            'real function f1()'//new_line('a')// &
            'end function'//new_line('a')// &
            'end interface'//new_line('a')// &
            'dimension :: f1(4)'//new_line('a')// &
            'end module m1'
        call expect_frontend_error(source, 'outside its INTERFACE body', passed)
    end subroutine test_attribute_outside_interface_body_rejected

    ! derived_function_interface_1: a contained procedure already has an
    ! explicit interface.
    subroutine test_contained_procedure_with_interface_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing INTERFACE body for a contained procedure (rejected)...'
        source = 'program main'//new_line('a')// &
            'interface'//new_line('a')// &
            'real function fun()'//new_line('a')// &
            'end function fun'//new_line('a')// &
            'end interface'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'contains'//new_line('a')// &
            'real function fun()'//new_line('a')// &
            'fun = 1.0'//new_line('a')// &
            'end function fun'//new_line('a')// &
            'end program main'
        call expect_frontend_error(source, 'already has an explicit interface', &
            passed)
    end subroutine test_contained_procedure_with_interface_rejected

    ! module_procedure_2: an INTRINSIC name is not a module procedure.
    subroutine test_intrinsic_as_module_procedure_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE naming an INTRINSIC (rejected)...'
        source = 'program test'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'intrinsic sin'//new_line('a')// &
            'interface gen2'//new_line('a')// &
            'module procedure sin'//new_line('a')// &
            'end interface gen2'//new_line('a')// &
            'end program test'
        call expect_frontend_error(source, 'cannot be a MODULE PROCEDURE', passed)
    end subroutine test_intrinsic_as_module_procedure_rejected

    ! assumed_charlen_function_6: assumed-length result of a module procedure.
    subroutine test_assumed_length_module_function_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing assumed-length result of a module function ' // &
            '(rejected)...'
        source = 'module funcs'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'function assumed_len(x)'//new_line('a')// &
            'character(*) assumed_len'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'end function assumed_len'//new_line('a')// &
            'end module funcs'
        call expect_frontend_error(source, 'has assumed length', passed)
    end subroutine test_assumed_length_module_function_rejected

    ! whole_file_16 and volatile14: the callee needs an explicit interface.
    subroutine test_call_without_required_interface_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing call to an assumed-shape procedure without an ' // &
            'interface (rejected)...'
        source = 'program main'//new_line('a')// &
            'real, dimension(2) :: a'//new_line('a')// &
            'call foo(a)'//new_line('a')// &
            'end program main'//new_line('a')// &
            'subroutine foo(a)'//new_line('a')// &
            'real, dimension(:) :: a'//new_line('a')// &
            'end subroutine foo'
        call expect_frontend_error(source, 'Explicit interface required', passed)
    end subroutine test_call_without_required_interface_rejected

    ! Corrected neighbour: EXTERNAL alone, with no interface body.
    subroutine test_external_statement_without_interface_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing EXTERNAL without an interface body (accepted)...'
        source = 'module a'//new_line('a')// &
            'external foo'//new_line('a')// &
            'intrinsic sin'//new_line('a')// &
            'end module a'
        call expect_frontend_accepts(source, passed)
    end subroutine test_external_statement_without_interface_accepted

    ! Corrected neighbour of interface_24: module m3 of the same fixture.
    subroutine test_attribute_inside_interface_body_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing attribute inside the INTERFACE body (accepted)...'
        source = 'module m3'//new_line('a')// &
            'interface'//new_line('a')// &
            'real function f3()'//new_line('a')// &
            'dimension :: f3(4)'//new_line('a')// &
            'end function'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m3'
        call expect_frontend_accepts(source, passed)
    end subroutine test_attribute_inside_interface_body_accepted

    ! A separate module procedure interface is defined in the same module.
    subroutine test_separate_module_procedure_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing separate module procedure definition (accepted)...'
        source = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'module subroutine work(x)'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'end subroutine work'//new_line('a')// &
            'end interface'//new_line('a')// &
            'contains'//new_line('a')// &
            'module subroutine work(x)'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'end subroutine work'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_separate_module_procedure_accepted

    ! Corrected neighbour: an external function may have assumed-length result.
    subroutine test_assumed_length_external_function_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing assumed-length result of an external function ' // &
            '(accepted)...'
        source = 'function assumed_len(x)'//new_line('a')// &
            'character(*) assumed_len'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'assumed_len = ""'//new_line('a')// &
            'end function assumed_len'
        call expect_frontend_accepts(source, passed)
    end subroutine test_assumed_length_external_function_accepted

    ! Corrected neighbour of whole_file_16: the interface is provided.
    subroutine test_call_with_interface_block_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing call with an explicit interface block (accepted)...'
        source = 'program main'//new_line('a')// &
            'real, dimension(2) :: a'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine foo(a)'//new_line('a')// &
            'real, dimension(:) :: a'//new_line('a')// &
            'end subroutine foo'//new_line('a')// &
            'end interface'//new_line('a')// &
            'call foo(a)'//new_line('a')// &
            'end program main'//new_line('a')// &
            'subroutine foo(a)'//new_line('a')// &
            'real, dimension(:) :: a'//new_line('a')// &
            'end subroutine foo'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_with_interface_block_accepted

    ! A USE-associated interface block is explicit in the importing scope.
    subroutine test_call_with_use_associated_interface_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing call with a USE-associated interface (accepted)...'
        source = 'module provider'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine foo(a)'//new_line('a')// &
            'real :: a(:)'//new_line('a')// &
            'end subroutine foo'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module provider'//new_line('a')// &
            'module caller'//new_line('a')// &
            'use provider'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine invoke(a)'//new_line('a')// &
            'real :: a(:)'//new_line('a')// &
            'call foo(a)'//new_line('a')// &
            'end subroutine invoke'//new_line('a')// &
            'end module caller'//new_line('a')// &
            'subroutine foo(a)'//new_line('a')// &
            'real :: a(:)'//new_line('a')// &
            'end subroutine foo'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_with_use_associated_interface_accepted

    subroutine expect_frontend_error(source, expected, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (result%success()) then
            print *, '  FAIL: invalid source was accepted'
            passed = .false.
            return
        end if
        if (index(result%error_msg, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%error_msg)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_error

    subroutine expect_frontend_accepts(source, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%success()) then
            print *, '  FAIL: valid source was rejected'
            if (allocated(result%error_msg)) print *, trim(result%error_msg)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_accepts

end program test_reject_interface_01_diagnostics
