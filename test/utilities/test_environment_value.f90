program test_environment_value
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: path_value
    character(len=:), allocatable :: path_value_again
    character(len=:), allocatable :: missing_value

    path_value = get_environment_value('PATH')
    if (len_trim(path_value) == 0) then
        write (error_unit, '(A)') 'FAIL: Expected PATH to be non-empty'
        stop 1
    end if

    path_value_again = get_environment_value('PATH')
    if (len_trim(path_value_again) == 0) then
        write (error_unit, '(A)') 'FAIL: Expected PATH to be non-empty on second call'
        stop 1
    end if

    missing_value = get_environment_value('FORTFRONT__MISSING_ENV__2711')
    if (len_trim(missing_value) /= 0) then
        write (error_unit, '(A)') &
            'FAIL: Expected missing env var to return empty string'
        stop 1
    end if

    print *, 'PASS: get_environment_value returns stable, safe results'

contains

    include '../common/filesystem_helpers.inc'

end program test_environment_value
