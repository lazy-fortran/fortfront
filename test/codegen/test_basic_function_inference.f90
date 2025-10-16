program test_basic_function_inference
    use frontend, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    if (run_basic_function_test()) then
        write (*, '(A)') 'PASS: basic function inference generates integer types'
        stop 0
    else
        write (error_unit, '(A)') 'FAIL: basic function inference generates integer types'
        stop 1
    end if

contains

    function run_basic_function_test() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors
        integer :: i
        character(len=32), parameter :: required_fragments(3) = (/ &
                                        'integer function square', &
                                        'integer :: x           ', &
                                        'integer :: val         '/)
        character(len=32), parameter :: forbidden_fragments(4) = (/ &
                                        'real function square      ', &
                                        'real :: square            ', &
                                        'real(8) function square   ', &
                                        'real, external :: square  '/)

        source = 'function square(x)' // new_line('a') // &
                 '    result = x * x' // new_line('a') // &
                 '    return result' // new_line('a') // &
                 'end function' // new_line('a') // new_line('a') // &
                 'val = 5' // new_line('a') // &
                 'squared = square(val)' // new_line('a') // &
                 'print *, squared'

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.

        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors:'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        do i = 1, size(required_fragments)
            if (index(generated, trim(required_fragments(i))) == 0) then
                write (error_unit, '(A)') 'missing fragment: ' // &
                    trim(required_fragments(i))
                passed = .false.
            end if
        end do

        do i = 1, size(forbidden_fragments)
            if (index(generated, trim(forbidden_fragments(i))) > 0) then
                write (error_unit, '(A)') 'unexpected fragment: ' // &
                    trim(forbidden_fragments(i))
                passed = .false.
            end if
        end do
    end function run_basic_function_test

end program test_basic_function_inference
