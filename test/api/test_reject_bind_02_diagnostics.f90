program test_reject_bind_02_diagnostics
    ! Issue #2886: BIND(C) interoperability contracts. A character dummy of a
    ! BIND(C) procedure must have length 1 (Fortran 2018 18.3.1), and the
    ! VALUE attribute must not be combined with POINTER, ALLOCATABLE,
    ! VOLATILE, INTENT(OUT) or INTENT(INOUT) (Fortran 2018 C863). Each
    ! rejected form is paired with a corrected neighbour.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== reject-bind-02: BIND(C) interoperability contracts ==='

    call test_character_dummy_length_four_rejected(all_tests_passed)
    call test_character_dummy_length_one_accepted(all_tests_passed)
    call test_assumed_length_character_dummy_rejected(all_tests_passed)
    call test_non_bind_c_character_dummy_accepted(all_tests_passed)
    call test_value_pointer_dummy_rejected(all_tests_passed)
    call test_value_allocatable_dummy_rejected(all_tests_passed)
    call test_value_intent_out_dummy_rejected(all_tests_passed)
    call test_value_intent_in_dummy_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All reject-bind-02 tests passed'
        stop 0
    else
        print *, 'Some reject-bind-02 tests failed'
        stop 1
    end if

contains

    subroutine test_character_dummy_length_four_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'BIND(C) character dummy of length 4 (rejected)...'
        source = 'subroutine put_text(text) bind(c)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=4) :: text'//new_line('a')// &
            'end subroutine put_text'
        call expect_frontend_error(source, 'length other than 1', passed)
    end subroutine test_character_dummy_length_four_rejected

    subroutine test_character_dummy_length_one_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'BIND(C) character dummy of length 1 (accepted)...'
        source = 'subroutine put_text(text) bind(c)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=1) :: text'//new_line('a')// &
            'end subroutine put_text'
        call expect_frontend_success(source, passed)
    end subroutine test_character_dummy_length_one_accepted

    subroutine test_assumed_length_character_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'BIND(C) assumed-length character dummy (rejected)...'
        source = 'subroutine put_text(text) bind(c, name="put_text_c")'// &
            new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=*) :: text'//new_line('a')// &
            'end subroutine put_text'
        call expect_frontend_error(source, 'length other than 1', passed)
    end subroutine test_assumed_length_character_dummy_rejected

    subroutine test_non_bind_c_character_dummy_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Plain Fortran character dummy of length 4 (accepted)...'
        source = 'subroutine put_text(text)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=4) :: text'//new_line('a')// &
            'end subroutine put_text'
        call expect_frontend_success(source, passed)
    end subroutine test_non_bind_c_character_dummy_accepted

    subroutine test_value_pointer_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'VALUE dummy with POINTER attribute (rejected)...'
        source = 'subroutine take(x)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, value, pointer :: x'//new_line('a')// &
            'end subroutine take'
        call expect_frontend_error(source, 'POINTER attribute', passed)
    end subroutine test_value_pointer_dummy_rejected

    subroutine test_value_allocatable_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'VALUE dummy with ALLOCATABLE attribute (rejected)...'
        source = 'subroutine take(x)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, value, allocatable :: x'//new_line('a')// &
            'end subroutine take'
        call expect_frontend_error(source, 'ALLOCATABLE attribute', passed)
    end subroutine test_value_allocatable_dummy_rejected

    subroutine test_value_intent_out_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'VALUE dummy with INTENT(OUT) (rejected)...'
        source = 'subroutine take(x)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, value, intent(out) :: x'//new_line('a')// &
            'end subroutine take'
        call expect_frontend_error(source, 'INTENT(OUT) attribute', passed)
    end subroutine test_value_intent_out_dummy_rejected

    subroutine test_value_intent_in_dummy_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'VALUE dummy with INTENT(IN) (accepted)...'
        source = 'subroutine take(x)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, value, intent(in) :: x'//new_line('a')// &
            'end subroutine take'
        call expect_frontend_success(source, passed)
    end subroutine test_value_intent_in_dummy_accepted

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

    subroutine expect_frontend_success(source, passed)
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
    end subroutine expect_frontend_success

end program test_reject_bind_02_diagnostics
