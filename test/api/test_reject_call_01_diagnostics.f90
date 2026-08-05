program test_reject_call_01_diagnostics
    ! Rejection coverage for procedure-call signature mismatches (issue #2882).
    !
    ! Rule under test: the procedure designator of a CALL statement must name a
    ! subroutine.  A name that has a type in an accessible scoping unit -- a
    ! data object, or a function -- is not a subroutine, so the CALL is invalid.
    ! Represented by gfortran.dg/typed_subroutine_1.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject invalid CALL targets (issue #2882) ==='

    call test_call_of_typed_variable_rejected(all_tests_passed)
    call test_call_of_multi_declared_variable_rejected(all_tests_passed)
    call test_call_of_contained_function_rejected(all_tests_passed)
    call test_call_of_contained_subroutine_accepted(all_tests_passed)
    call test_call_of_external_declared_name_accepted(all_tests_passed)
    call test_call_of_intrinsic_subroutine_accepted(all_tests_passed)
    call test_call_of_undeclared_name_accepted(all_tests_passed)
    call test_call_of_host_associated_function_rejected(all_tests_passed)
    call test_call_of_host_associated_subroutine_accepted(all_tests_passed)
    call test_interface_body_argument_type_rejected(all_tests_passed)
    call test_interface_body_argument_type_accepted(all_tests_passed)
    call test_intrinsic_actual_argument_arity_rejected(all_tests_passed)
    call test_intrinsic_actual_argument_arity_accepted(all_tests_passed)
    call test_external_actual_for_function_dummy_rejected(all_tests_passed)
    call test_function_actual_for_function_dummy_accepted(all_tests_passed)
    call test_dummy_procedure_intent_mismatch_rejected(all_tests_passed)
    call test_dummy_procedure_intent_match_accepted(all_tests_passed)
    call test_dummy_procedure_optional_mismatch_rejected(all_tests_passed)
    call test_function_dummy_type_mismatch_rejected(all_tests_passed)
    call test_external_result_type_mismatch_rejected(all_tests_passed)
    call test_external_result_type_match_accepted(all_tests_passed)
    call test_impure_call_in_do_concurrent_rejected(all_tests_passed)
    call test_pure_call_in_do_concurrent_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All CALL target rejection tests passed'
        stop 0
    else
        print *, 'Some CALL target rejection tests failed'
        stop 1
    end if

contains

    include '../common/read_example.inc'

    subroutine test_call_of_typed_variable_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a typed local variable (rejected)...'
        source = 'integer :: s'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'has a type', passed)
    end subroutine test_call_of_typed_variable_rejected

    subroutine test_call_of_multi_declared_variable_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a name from a multi declaration (rejected)...'
        source = 'integer :: a, s, b'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'not consistent with the CALL', passed)
    end subroutine test_call_of_multi_declared_variable_rejected

    subroutine test_call_of_contained_function_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a contained function (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call f()'//new_line('a')// &
            'contains'//new_line('a')// &
            'function f() result(v)'//new_line('a')// &
            'real :: v'//new_line('a')// &
            'v = 1'//new_line('a')// &
            'end function f'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'has a type', passed)
    end subroutine test_call_of_contained_function_rejected

    subroutine test_call_of_contained_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a contained subroutine (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call f()'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine f()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_contained_subroutine_accepted

    subroutine test_call_of_external_declared_name_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an EXTERNAL declared name (accepted)...'
        source = 'integer, external :: s'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_external_declared_name_accepted

    subroutine test_call_of_intrinsic_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an intrinsic subroutine (accepted)...'
        source = 'real :: random_number_result'//new_line('a')// &
            'call random_number(random_number_result)'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_intrinsic_subroutine_accepted

    subroutine test_call_of_undeclared_name_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an undeclared external name (accepted)...'
        source = 'call s()'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_undeclared_name_accepted


    subroutine test_call_of_host_associated_function_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a host-associated module function (rejected)...'
        source = 'module diatoms'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'function initial_x() result(v4)'//new_line('a')// &
            'real :: v4'//new_line('a')// &
            'v4 = 1'//new_line('a')// &
            'end function initial_x'//new_line('a')// &
            'subroutine find_period()'//new_line('a')// &
            'call initial_x()'//new_line('a')// &
            'end subroutine find_period'//new_line('a')// &
            'end module diatoms'
        call expect_frontend_error(source, 'not consistent with the CALL', passed)
    end subroutine test_call_of_host_associated_function_rejected

    subroutine test_call_of_host_associated_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a host-associated module subroutine (accepted)...'
        source = 'module diatoms'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine initial_x()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end subroutine initial_x'//new_line('a')// &
            'subroutine find_period()'//new_line('a')// &
            'call initial_x()'//new_line('a')// &
            'end subroutine find_period'//new_line('a')// &
            'end module diatoms'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_host_associated_subroutine_accepted

    subroutine test_interface_body_argument_type_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing argument type against a local interface body (rejected)...'
        source = 'subroutine h()'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine f(a, b)'//new_line('a')// &
            'integer :: a'//new_line('a')// &
            'character(*) :: b'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end interface'//new_line('a')// &
            'call f(6, 6.0)'//new_line('a')// &
            'end subroutine h'
        call expect_frontend_error(source, 'Type mismatch in call', passed)
    end subroutine test_interface_body_argument_type_rejected

    subroutine test_interface_body_argument_type_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing argument type against a local interface body (accepted)...'
        source = 'subroutine h()'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine f(a, b)'//new_line('a')// &
            'integer :: a'//new_line('a')// &
            'character(*) :: b'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end interface'//new_line('a')// &
            'call f(6, "abcdef")'//new_line('a')// &
            'end subroutine h'
        call expect_frontend_accepts(source, passed)
    end subroutine test_interface_body_argument_type_accepted

    subroutine test_intrinsic_actual_argument_arity_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing intrinsic actual with wrong argument count (rejected)...'
        source = dummy_function_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'intrinsic cos'//new_line('a')// &
            'call sub(cos)'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'wrong number of arguments', passed)
    end subroutine test_intrinsic_actual_argument_arity_rejected

    subroutine test_intrinsic_actual_argument_arity_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing intrinsic actual with matching argument count (accepted)...'
        source = unary_dummy_function_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'intrinsic cos'//new_line('a')// &
            'call sub(cos)'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_intrinsic_actual_argument_arity_accepted

    subroutine test_external_actual_for_function_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing untyped EXTERNAL actual for a function dummy (rejected)...'
        source = dummy_function_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'external foo'//new_line('a')// &
            'call sub(foo)'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'is not a function', passed)
    end subroutine test_external_actual_for_function_dummy_rejected

    subroutine test_function_actual_for_function_dummy_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing function actual for a function dummy (accepted)...'
        source = dummy_function_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call sub(foo)'//new_line('a')// &
            'contains'//new_line('a')// &
            'function foo()'//new_line('a')// &
            'real :: foo'//new_line('a')// &
            'foo = 1.0'//new_line('a')// &
            'end function foo'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_function_actual_for_function_dummy_accepted

    ! Module whose subroutine `sub` takes a dummy procedure declared by an
    ! interface body as a function of no arguments.
    function dummy_function_module() result(text)
        character(len=:), allocatable :: text

        text = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub(a)'//new_line('a')// &
            'interface'//new_line('a')// &
            'function a()'//new_line('a')// &
            'real :: a'//new_line('a')// &
            'end function a'//new_line('a')// &
            'end interface'//new_line('a')// &
            'print *, a()'//new_line('a')// &
            'end subroutine sub'//new_line('a')// &
            'end module m'
    end function dummy_function_module

    ! As dummy_function_module, but the dummy function takes one argument, so
    ! an intrinsic actual with matching arity is accepted.
    function unary_dummy_function_module() result(text)
        character(len=:), allocatable :: text

        text = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub(a)'//new_line('a')// &
            'interface'//new_line('a')// &
            'function a(x)'//new_line('a')// &
            'real :: a, x'//new_line('a')// &
            'end function a'//new_line('a')// &
            'end interface'//new_line('a')// &
            'print *, a(4.0)'//new_line('a')// &
            'end subroutine sub'//new_line('a')// &
            'end module m'
    end function unary_dummy_function_module

    ! Module whose subroutine `test` takes a dummy subroutine with one
    ! INTENT(IN), OPTIONAL integer argument.
    function dummy_subroutine_module() result(text)
        character(len=:), allocatable :: text

        text = 'module testsub'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine test(sub)'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine sub(x)'//new_line('a')// &
            'integer, intent(in), optional :: x'//new_line('a')// &
            'end subroutine sub'//new_line('a')// &
            'end interface'//new_line('a')// &
            'call sub()'//new_line('a')// &
            'end subroutine test'//new_line('a')// &
            'end module testsub'
    end function dummy_subroutine_module

    subroutine test_dummy_procedure_intent_mismatch_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing dummy procedure INTENT mismatch (rejected)...'
        source = dummy_subroutine_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use testsub'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call test(sub_actual)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub_actual(x)'//new_line('a')// &
            'integer, intent(inout), optional :: x'//new_line('a')// &
            'if (present(x)) x = 1'//new_line('a')// &
            'end subroutine sub_actual'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'INTENT mismatch in argument', passed)
    end subroutine test_dummy_procedure_intent_mismatch_rejected

    subroutine test_dummy_procedure_intent_match_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing dummy procedure INTENT match (accepted)...'
        source = dummy_subroutine_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use testsub'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call test(sub_actual)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub_actual(x)'//new_line('a')// &
            'integer, intent(in), optional :: x'//new_line('a')// &
            'if (present(x)) print *, x'//new_line('a')// &
            'end subroutine sub_actual'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_dummy_procedure_intent_match_accepted

    subroutine test_dummy_procedure_optional_mismatch_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing dummy procedure OPTIONAL mismatch (rejected)...'
        source = dummy_subroutine_module()//new_line('a')// &
            'program p'//new_line('a')// &
            'use testsub'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call test(sub_actual)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub_actual(x)'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'print *, x'//new_line('a')// &
            'end subroutine sub_actual'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'OPTIONAL mismatch in argument', passed)
    end subroutine test_dummy_procedure_optional_mismatch_rejected

    subroutine test_function_dummy_type_mismatch_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing function dummy argument type mismatch (rejected)...'
        call read_example( &
            'examples/f90/reject_call_function_dummy_type.f90', source)
        call expect_frontend_error(source, 'Type mismatch in argument', passed)
    end subroutine test_function_dummy_type_mismatch_rejected

    subroutine test_external_result_type_mismatch_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing external function result type mismatch (rejected)...'
        source = 'program main'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'type t'//new_line('a')// &
            'integer :: g'//new_line('a')// &
            'end type t'//new_line('a')// &
            'type u'//new_line('a')// &
            'integer :: g'//new_line('a')// &
            'end type u'//new_line('a')// &
            'type(u), external :: ufunc'//new_line('a')// &
            'call sub(ufunc)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub(tfunc)'//new_line('a')// &
            'type(t), external :: tfunc'//new_line('a')// &
            'end subroutine sub'//new_line('a')// &
            'end program main'
        call expect_frontend_error(source, 'Type mismatch in function result', &
            passed)
    end subroutine test_external_result_type_mismatch_rejected

    subroutine test_external_result_type_match_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing external function result type match (accepted)...'
        source = 'program main'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'type t'//new_line('a')// &
            'integer :: g'//new_line('a')// &
            'end type t'//new_line('a')// &
            'type(t), external :: tfunc'//new_line('a')// &
            'call sub(tfunc)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine sub(pfunc)'//new_line('a')// &
            'type(t), external :: pfunc'//new_line('a')// &
            'end subroutine sub'//new_line('a')// &
            'end program main'
        call expect_frontend_accepts(source, passed)
    end subroutine test_external_result_type_match_accepted

    subroutine test_impure_call_in_do_concurrent_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing impure subroutine call in DO CONCURRENT (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: i'//new_line('a')// &
            'integer :: y(4)'//new_line('a')// &
            'do concurrent (i=1:4)'//new_line('a')// &
            'call bar(y)'//new_line('a')// &
            'end do'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine bar(y)'//new_line('a')// &
            'integer, intent(out) :: y(:)'//new_line('a')// &
            'y = 1'//new_line('a')// &
            'end subroutine bar'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'DO CONCURRENT', passed)
    end subroutine test_impure_call_in_do_concurrent_rejected

    subroutine test_pure_call_in_do_concurrent_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing pure subroutine call in DO CONCURRENT (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: i'//new_line('a')// &
            'integer :: y(4)'//new_line('a')// &
            'do concurrent (i=1:4)'//new_line('a')// &
            'call bar(y)'//new_line('a')// &
            'end do'//new_line('a')// &
            'contains'//new_line('a')// &
            'pure subroutine bar(y)'//new_line('a')// &
            'integer, intent(out) :: y(:)'//new_line('a')// &
            'y = 1'//new_line('a')// &
            'end subroutine bar'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_pure_call_in_do_concurrent_accepted

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
        if (index(result%diagnostic_text, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%diagnostic_text)
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
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_accepts

end program test_reject_call_01_diagnostics
