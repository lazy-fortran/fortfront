program test_issue_1343_lazy_function_inference
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1343: Lazy function inference ==='

    if (.not. test_lazy_function_inference()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1343 fixed!'
    else
        print *, 'Issue #1343 regression detected!'
        stop 1
    end if

contains

    logical function test_lazy_function_inference()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_signature
        logical :: has_param_types
        logical :: has_result_decl

        test_lazy_function_inference = .true.
        print *, 'Testing lazy function inference...'

        call read_example('examples/lf/issue_1343_lazy_function_inference.lf', &
                          source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_lazy_function_inference = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_lazy_function_inference = .false.
            return
        end if

        has_signature = index(output, 'integer function add(') > 0
        has_param_types = index(output, 'integer, intent(in) :: a') > 0 .and. &
                          index(output, 'integer, intent(in) :: b') > 0
        has_result_decl = index(output, 'result(result)') > 0

        if (.not. has_signature) then
            print *, '  FAIL: Function signature not inferred as integer'
            test_lazy_function_inference = .false.
        end if

        if (.not. has_param_types) then
            print *, '  FAIL: Parameter types not inferred as integer'
            test_lazy_function_inference = .false.
        end if

        if (.not. has_result_decl) then
            print *, '  FAIL: Result variable missing integer declaration'
            test_lazy_function_inference = .false.
        end if

        if (test_lazy_function_inference) then
            print *, '  PASS: Lazy function parameters and result inferred'
        end if
    end function test_lazy_function_inference

    include '../../common/read_example.inc'
end program test_issue_1343_lazy_function_inference
