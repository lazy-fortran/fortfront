program test_intrinsic_registry
    use intrinsic_registry
    implicit none

    logical :: is_intrinsic
    character(len=:), allocatable :: signature
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0

    print *, "Testing intrinsic function registry..."

    ! Test mathematical functions
    call test_intrinsic_function("abs", .true., "numeric(numeric)", test_count, passed_count)
    call test_intrinsic_function("sqrt", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("sin", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("cos", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("exp", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("log", .true., "real(numeric)", test_count, passed_count)

    ! Test type conversion functions
    call test_intrinsic_function("int", .true., "integer(numeric)", test_count, passed_count)
    call test_intrinsic_function("real", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("dble", .true., "double(numeric)", test_count, passed_count)

    ! Test array functions
    call test_intrinsic_function("size", .true., "integer(array,integer?)", test_count, passed_count)
    call test_intrinsic_function("shape", .true., "integer_array(array)", test_count, passed_count)
    call test_intrinsic_function("sum", .true., "numeric(array,integer?)", test_count, passed_count)

    ! Test string functions
    call test_intrinsic_function("len", .true., "integer(character)", test_count, passed_count)
    call test_intrinsic_function("trim", .true., "character(character)", test_count, passed_count)

    ! Test case insensitivity
    call test_intrinsic_function("ABS", .true., "numeric(numeric)", test_count, passed_count)
    call test_intrinsic_function("SiN", .true., "real(numeric)", test_count, passed_count)
    call test_intrinsic_function("LEN", .true., "integer(character)", test_count, passed_count)

    ! Test non-intrinsic functions
    call test_intrinsic_function("user_function", .false., "", test_count, passed_count)
    call test_intrinsic_function("my_sqrt", .false., "", test_count, passed_count)
    call test_intrinsic_function("custom_func", .false., "", test_count, passed_count)

    ! Test get_intrinsic_info combined function
    call get_intrinsic_info("abs", is_intrinsic, signature)
    test_count = test_count + 1
    if (is_intrinsic .and. signature == "numeric(numeric)") then
        passed_count = passed_count + 1
        print *, "PASS: get_intrinsic_info for 'abs'"
    else
        print *, "FAIL: get_intrinsic_info for 'abs', got:", is_intrinsic, "|", signature, "|"
    end if

    call get_intrinsic_info("non_intrinsic", is_intrinsic, signature)
    test_count = test_count + 1
    if (.not. is_intrinsic .and. signature == "") then
        passed_count = passed_count + 1
        print *, "PASS: get_intrinsic_info for non-intrinsic"
    else
        print *, "FAIL: get_intrinsic_info for non-intrinsic, got:", is_intrinsic, "|", signature, "|"
    end if

    ! Summary
    print *, ""
    print *, "Test Results:"
    print *, "  Total tests: ", test_count
    print *, "  Passed:      ", passed_count
    print *, "  Failed:      ", test_count - passed_count
    
    if (passed_count == test_count) then
        print *, "All tests passed!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if

contains

    subroutine test_intrinsic_function(name, expected_intrinsic, expected_signature, &
                                       test_count, passed_count)
        character(len=*), intent(in) :: name, expected_signature
        logical, intent(in) :: expected_intrinsic
        integer, intent(inout) :: test_count, passed_count
        logical :: is_intrinsic
        character(len=:), allocatable :: signature

        test_count = test_count + 1

        is_intrinsic = is_intrinsic_function(name)
        signature = get_intrinsic_signature(name)

        if (is_intrinsic .eqv. expected_intrinsic .and. &
            trim(signature) == trim(expected_signature)) then
            passed_count = passed_count + 1
            print *, "PASS: ", trim(name), " -> intrinsic=", is_intrinsic, &
                     ", signature=", trim(signature)
        else
            print *, "FAIL: ", trim(name), " expected intrinsic=", expected_intrinsic, &
                     ", signature='", trim(expected_signature), "'"
            print *, "      got intrinsic=", is_intrinsic, &
                     ", signature='", trim(signature), "'"
        end if
    end subroutine test_intrinsic_function

end program test_intrinsic_registry