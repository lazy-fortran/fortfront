program test_issue_1816_multiple_returns
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1816: Multiple return values ==='

    if (.not. test_result_variable_detection()) all_passed = .false.
    if (.not. test_local_variable_declarations()) all_passed = .false.
    if (.not. test_array_result_type_inference()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1816 fixed!'
    else
        print *, 'Issue #1816 regression detected!'
        stop 1
    end if

contains

    logical function test_result_variable_detection()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_result_clause
        logical :: not_using_q_as_result

        test_result_variable_detection = .true.
        print *, 'Testing result variable detection...'

        source = 'function divmod(a, b)' // new_line('a') // &
                 '    q = a / b' // new_line('a') // &
                 '    r = mod(a, b)' // new_line('a') // &
                 '    result = [q, r]' // new_line('a') // &
                 'end function' // new_line('a') // &
                 '' // new_line('a') // &
                 'res = divmod(17, 5)' // new_line('a') // &
                 'print *, res'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_result_variable_detection = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_result_variable_detection = .false.
            return
        end if

        has_result_clause = index(output, 'result(result)') > 0
        not_using_q_as_result = index(output, 'result(q)') == 0

        if (.not. has_result_clause) then
            print *, '  FAIL: Result variable not detected as result'
            test_result_variable_detection = .false.
        end if

        if (.not. not_using_q_as_result) then
            print *, '  FAIL: Incorrectly using q as result variable'
            test_result_variable_detection = .false.
        end if

        if (test_result_variable_detection) then
            print *, '  PASS: Result variable correctly detected'
        end if
    end function test_result_variable_detection

    logical function test_local_variable_declarations()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_q_decl
        logical :: has_r_decl

        test_local_variable_declarations = .true.
        print *, 'Testing local variable declarations...'

        source = 'function divmod(a, b)' // new_line('a') // &
                 '    q = a / b' // new_line('a') // &
                 '    r = mod(a, b)' // new_line('a') // &
                 '    result = [q, r]' // new_line('a') // &
                 'end function' // new_line('a') // &
                 '' // new_line('a') // &
                 'res = divmod(17, 5)' // new_line('a') // &
                 'print *, res'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_local_variable_declarations = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_local_variable_declarations = .false.
            return
        end if

        has_q_decl = index(output, ':: q') > 0
        has_r_decl = index(output, ':: r') > 0

        if (.not. has_q_decl) then
            print *, '  FAIL: Local variable q not declared'
            test_local_variable_declarations = .false.
        end if

        if (.not. has_r_decl) then
            print *, '  FAIL: Local variable r not declared'
            test_local_variable_declarations = .false.
        end if

        if (test_local_variable_declarations) then
            print *, '  PASS: Local variables declared'
        end if
    end function test_local_variable_declarations

    logical function test_array_result_type_inference()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_array_result

        test_array_result_type_inference = .true.
        print *, 'Testing array result type inference...'

        source = 'function divmod(a, b)' // new_line('a') // &
                 '    q = a / b' // new_line('a') // &
                 '    r = mod(a, b)' // new_line('a') // &
                 '    result = [q, r]' // new_line('a') // &
                 'end function' // new_line('a') // &
                 '' // new_line('a') // &
                 'res = divmod(17, 5)' // new_line('a') // &
                 'print *, res'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_array_result_type_inference = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_array_result_type_inference = .false.
            return
        end if

        has_array_result = index(output, 'dimension(2)') > 0 .or. &
                           index(output, ', dimension (2)') > 0

        if (.not. has_array_result) then
            print *, '  FAIL: Result type not inferred as array'
            test_array_result_type_inference = .false.
        end if

        if (test_array_result_type_inference) then
            print *, '  PASS: Array result type inferred'
        end if
    end function test_array_result_type_inference

end program test_issue_1816_multiple_returns
