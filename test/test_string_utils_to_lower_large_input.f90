program test_string_utils_to_lower_large_input
    use, intrinsic :: iso_fortran_env, only: error_unit
    use string_utils_mod, only: to_lower
    implicit none

    integer, parameter :: n = 2000000
    character(len=:), allocatable :: text
    character(len=:), allocatable :: lowered

    allocate (character(len=n) :: text)
    text = repeat('A', n)

    if (len(text) /= n) then
        write (error_unit, '(A,I0,A,I0,A)') 'FAIL: unexpected input length (', &
            len(text), '), expected (', n, ')'
        error stop 1
    end if

    lowered = to_lower(text)

    if (.not. allocated(lowered)) then
        write (error_unit, '(A)') 'FAIL: to_lower returned unallocated string'
        error stop 1
    end if

    if (len(lowered) /= n) then
        write (error_unit, '(A,I0,A,I0,A)') 'FAIL: unexpected length (', &
            len(lowered), '), expected (', n, ')'
        error stop 1
    end if

    if (lowered(1:1) /= 'a') then
        write (error_unit, '(A)') 'FAIL: first character not lowercased'
        error stop 1
    end if

    if (lowered(n:n) /= 'a') then
        write (error_unit, '(A)') 'FAIL: last character not lowercased'
        error stop 1
    end if

    print *, 'PASS: to_lower handles large input'
end program test_string_utils_to_lower_large_input
