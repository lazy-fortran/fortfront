program test_error_api
    use error_api, only: &
        error_record_t, &
        error_collection_t, &
        error_context_t, &
        create_error_context, &
        format_error_message, &
        ERROR_INFO, &
        ERROR_WARNING, &
        ERROR_ERROR, &
        ERROR_FATAL
    implicit none

    logical :: all_passed

    print *, '=== Error API Tests ==='
    print *

    all_passed = .true.
    if (.not. test_error_context_creation()) all_passed = .false.
    if (.not. test_error_collection_basic()) all_passed = .false.
    if (.not. test_error_severity_levels()) all_passed = .false.
    if (.not. test_error_formatting()) all_passed = .false.
    if (.not. test_error_collection_add_with_context()) all_passed = .false.
    if (.not. test_error_collection_format_and_clear()) all_passed = .false.
    if (.not. test_error_collection_format_long_message()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All error API tests passed!'
        stop 0
    else
        print *, 'Error API tests failed!'
        stop 1
    end if

contains

    logical function test_error_context_creation()
        type(error_context_t) :: ctx

        test_error_context_creation = .true.
        print *, 'Testing error context creation...'

        ctx = create_error_context(10, 5)

        if (ctx%line /= 10) then
            print *, '  FAIL: line not set correctly'
            test_error_context_creation = .false.
            return
        end if

        if (ctx%column /= 5) then
            print *, '  FAIL: column not set correctly'
            test_error_context_creation = .false.
            return
        end if

        ctx = create_error_context(15, 20, filename='test.f90')

        if (.not. allocated(ctx%filename)) then
            print *, '  FAIL: filename not allocated'
            test_error_context_creation = .false.
            return
        end if

        if (ctx%filename /= 'test.f90') then
            print *, '  FAIL: filename not set correctly'
            test_error_context_creation = .false.
            return
        end if

        print *, '  PASS: Error context creation'
    end function test_error_context_creation

    logical function test_error_collection_basic()
        type(error_collection_t) :: errors

        test_error_collection_basic = .true.
        print *, 'Testing error collection basic operations...'

        if (errors%has_errors()) then
            print *, '  FAIL: new collection should have no errors'
            test_error_collection_basic = .false.
            return
        end if

        call errors%add_error('Test error message')

        if (.not. errors%has_errors()) then
            print *, '  FAIL: collection should have errors after adding'
            test_error_collection_basic = .false.
            return
        end if

        if (errors%count /= 1) then
            print *, '  FAIL: error count should be 1'
            test_error_collection_basic = .false.
            return
        end if

        if (.not. allocated(errors%errors(1)%message)) then
            print *, '  FAIL: error message not allocated'
            test_error_collection_basic = .false.
            return
        end if

        if (errors%errors(1)%message /= 'Test error message') then
            print *, '  FAIL: error message not stored correctly'
            test_error_collection_basic = .false.
            return
        end if

        print *, '  PASS: Error collection basic operations'
    end function test_error_collection_basic

    logical function test_error_severity_levels()
        type(error_collection_t) :: errors

        test_error_severity_levels = .true.
        print *, 'Testing error severity levels...'

        call errors%add_error('Info message', severity=ERROR_INFO)
        if (errors%errors(1)%severity /= ERROR_INFO) then
            print *, '  FAIL: ERROR_INFO not set correctly'
            test_error_severity_levels = .false.
            return
        end if

        call errors%add_error('Warning message', severity=ERROR_WARNING)
        if (errors%errors(2)%severity /= ERROR_WARNING) then
            print *, '  FAIL: ERROR_WARNING not set correctly'
            test_error_severity_levels = .false.
            return
        end if

        call errors%add_error('Error message', severity=ERROR_ERROR)
        if (errors%errors(3)%severity /= ERROR_ERROR) then
            print *, '  FAIL: ERROR_ERROR not set correctly'
            test_error_severity_levels = .false.
            return
        end if

        call errors%add_error('Fatal message', severity=ERROR_FATAL)
        if (errors%errors(4)%severity /= ERROR_FATAL) then
            print *, '  FAIL: ERROR_FATAL not set correctly'
            test_error_severity_levels = .false.
            return
        end if

        if (.not. errors%has_fatal) then
            print *, '  FAIL: has_fatal flag not set'
            test_error_severity_levels = .false.
            return
        end if

        print *, '  PASS: Error severity levels'
    end function test_error_severity_levels

    logical function test_error_formatting()
        type(error_record_t) :: error
        type(error_context_t) :: ctx
        character(len=:), allocatable :: formatted

        test_error_formatting = .true.
        print *, 'Testing error message formatting...'

        error%message = 'Test error'
        error%severity = ERROR_ERROR

        formatted = format_error_message(error)

        if (.not. allocated(formatted)) then
            print *, '  FAIL: formatted message not allocated'
            test_error_formatting = .false.
            return
        end if

        if (index(formatted, 'ERROR') == 0) then
            print *, '  FAIL: severity not in formatted message'
            test_error_formatting = .false.
            return
        end if

        if (index(formatted, 'Test error') == 0) then
            print *, '  FAIL: error message not in formatted output'
            test_error_formatting = .false.
            return
        end if

        ctx = create_error_context(10, 5, filename='test.f90')
        error%context = ctx

        formatted = format_error_message(error)

        if (index(formatted, 'line 10') == 0) then
            print *, '  FAIL: line number not in formatted message'
            test_error_formatting = .false.
            return
        end if

        print *, '  PASS: Error message formatting'
    end function test_error_formatting

    logical function test_error_collection_add_with_context()
        type(error_collection_t) :: errors
        type(error_context_t) :: ctx

        test_error_collection_add_with_context = .true.
        print *, 'Testing error collection add with context...'

        ctx = create_error_context(5, 10, filename='sample.f90')
        call errors%add_error_with_context('Context error', ctx, &
                                           severity=ERROR_WARNING)

        if (errors%count /= 1) then
            print *, '  FAIL: add_error_with_context did not add error'
            test_error_collection_add_with_context = .false.
            return
        end if

        if (errors%errors(1)%context%line /= 5) then
            print *, '  FAIL: context not stored correctly'
            test_error_collection_add_with_context = .false.
            return
        end if

        call errors%add_error('Second error', suggestion='Try fixing it')

        if (.not. allocated(errors%errors(2)%suggestion)) then
            print *, '  FAIL: suggestion not stored'
            test_error_collection_add_with_context = .false.
            return
        end if

        print *, '  PASS: Error collection add with context'
    end function test_error_collection_add_with_context

    logical function test_error_collection_format_and_clear()
        type(error_collection_t) :: errors
        type(error_context_t) :: ctx
        character(len=:), allocatable :: formatted

        test_error_collection_format_and_clear = .true.
        print *, 'Testing error collection formatting and clear...'

        ctx = create_error_context(5, 10, filename='sample.f90')
        call errors%add_error_with_context('Context error', ctx, &
                                           severity=ERROR_WARNING)
        call errors%add_error('Second error', suggestion='Try fixing it')

        formatted = errors%format_messages()

        if (.not. allocated(formatted)) then
            print *, '  FAIL: format_messages did not return result'
            test_error_collection_format_and_clear = .false.
            return
        end if

        if (index(formatted, 'Context error') == 0) then
            print *, '  FAIL: first error not in formatted messages'
            test_error_collection_format_and_clear = .false.
            return
        end if

        if (index(formatted, 'Second error') == 0) then
            print *, '  FAIL: second error not in formatted messages'
            test_error_collection_format_and_clear = .false.
            return
        end if

        call errors%clear()

        if (errors%has_errors()) then
            print *, '  FAIL: collection not cleared'
            test_error_collection_format_and_clear = .false.
            return
        end if

        if (errors%count /= 0) then
            print *, '  FAIL: count not reset after clear'
            test_error_collection_format_and_clear = .false.
            return
        end if

        print *, '  PASS: Error collection formatting and clear'
    end function test_error_collection_format_and_clear

    ! Regression for #2831: a formatted message longer than the former
    ! fixed 1000-char fragment buffer must round-trip without truncation.
    logical function test_error_collection_format_long_message()
        type(error_collection_t) :: errors
        character(len=:), allocatable :: formatted
        character(len=2000) :: long_message

        test_error_collection_format_long_message = .true.
        print *, 'Testing error collection long-message formatting...'

        long_message = repeat('A', 1500) // 'TAIL'
        call errors%add_error(trim(long_message))

        formatted = errors%format_messages()

        if (.not. allocated(formatted)) then
            print *, '  FAIL: format_messages did not return result'
            test_error_collection_format_long_message = .false.
            return
        end if

        if (index(formatted, 'TAIL') == 0) then
            print *, '  FAIL: long message truncated, tail lost'
            test_error_collection_format_long_message = .false.
            return
        end if

        if (len(formatted) < 1500) then
            print *, '  FAIL: formatted message shorter than input'
            test_error_collection_format_long_message = .false.
            return
        end if

        print *, '  PASS: Error collection long-message formatting'
    end function test_error_collection_format_long_message

end program test_error_api
