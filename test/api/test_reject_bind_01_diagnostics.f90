program test_reject_bind_01_diagnostics
    ! Issue #2884: duplicate global BIND(C) binding labels must be rejected
    ! (Fortran 2018 C1553 - a binding label shall not be the same as the
    ! binding label of any other global entity). Each rejected form is
    ! paired with a corrected neighbour that must stay accepted.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== reject-bind-01: duplicate global BIND(C) labels ==='

    call test_duplicate_procedure_labels_rejected(all_tests_passed)
    call test_distinct_procedure_labels_accepted(all_tests_passed)
    call test_variable_and_procedure_label_clash_rejected(all_tests_passed)
    call test_distinct_variable_and_procedure_labels_accepted(all_tests_passed)
    call test_default_labels_accepted(all_tests_passed)
    call test_repeated_entity_label_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All reject-bind-01 tests passed'
        stop 0
    else
        print *, 'Some reject-bind-01 tests failed'
        stop 1
    end if

contains

    subroutine test_duplicate_procedure_labels_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Two procedures sharing a binding label (rejected)...'
        source = 'module dup_labels'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c, name="shared_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'subroutine beta() bind(c, name="shared_symbol")'//new_line('a')// &
            'end subroutine beta'//new_line('a')// &
            'end module dup_labels'
        call expect_frontend_error(source, 'binding label "shared_symbol"', passed)
    end subroutine test_duplicate_procedure_labels_rejected

    subroutine test_distinct_procedure_labels_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Two procedures with distinct binding labels (accepted)...'
        source = 'module ok_labels'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c, name="alpha_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'subroutine beta() bind(c, name="beta_symbol")'//new_line('a')// &
            'end subroutine beta'//new_line('a')// &
            'end module ok_labels'
        call expect_frontend_success(source, passed)
    end subroutine test_distinct_procedure_labels_accepted

    subroutine test_variable_and_procedure_label_clash_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Variable and procedure sharing a binding label (rejected)...'
        source = 'module dup_var_label'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, bind(c, name="shared_symbol") :: counter'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c, name="shared_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'end module dup_var_label'
        call expect_frontend_error(source, 'binding label "shared_symbol"', passed)
    end subroutine test_variable_and_procedure_label_clash_rejected

    subroutine test_distinct_variable_and_procedure_labels_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Variable and procedure with distinct labels (accepted)...'
        source = 'module ok_var_label'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, bind(c, name="counter_symbol") :: counter'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c, name="alpha_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'end module ok_var_label'
        call expect_frontend_success(source, passed)
    end subroutine test_distinct_variable_and_procedure_labels_accepted

    subroutine test_default_labels_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Distinct entities using default binding labels (accepted)...'
        source = 'module default_labels'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, bind(c) :: counter'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c)'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'end module default_labels'
        call expect_frontend_success(source, passed)
    end subroutine test_default_labels_accepted

    subroutine test_repeated_entity_label_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Interface body and definition of one entity (accepted)...'
        source = 'module repeated_entity'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine alpha() bind(c, name="alpha_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module repeated_entity'//new_line('a')// &
            'subroutine alpha() bind(c, name="alpha_symbol")'//new_line('a')// &
            'end subroutine alpha'
        call expect_frontend_success(source, passed)
    end subroutine test_repeated_entity_label_accepted

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

end program test_reject_bind_01_diagnostics
