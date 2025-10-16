program test_basic_function_inference
    use frontend, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    if (run_basic_function_test()) then
        write (*, '(A)') 'PASS: basic function inference emits integer types'
        stop 0
    else
        write (error_unit, '(A)') &
            & 'FAIL: basic function inference skipped integer emission'
        stop 1
    end if

contains

    function run_basic_function_test() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors
        character(len=32), parameter :: required_fragments(3) = [ &
                                        character(len=32) :: &
                                        'integer function square', &
                                        'integer :: x', &
                                        'integer :: val']
        character(len=32), parameter :: forbidden_fragments(4) = [ &
                                        character(len=32) :: &
                                        'real function square', &
                                        'real :: square', &
                                        'real(8) function square', &
                                        'real, external :: square']

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

        if (.not. verify_fragments(generated, required_fragments, .true.)) then
            passed = .false.
        end if
        if (.not. verify_fragments(generated, forbidden_fragments, .false.)) then
            passed = .false.
        end if
    end function run_basic_function_test

    logical function verify_fragments(generated, fragments, expect_present) &
        result(all_valid)
        character(len=*), intent(in) :: generated
        character(len=*), intent(in) :: fragments(:)
        logical, intent(in) :: expect_present
        integer :: i
        character(len=:), allocatable :: fragment

        all_valid = .true.
        do i = 1, size(fragments)
            fragment = trim(fragments(i))
            if (expect_present) then
                if (index(generated, fragment) == 0) then
                    write (error_unit, '(A)') 'missing fragment: ' // fragment
                    all_valid = .false.
                end if
            else
                if (index(generated, fragment) > 0) then
                    write (error_unit, '(A)') 'unexpected fragment: ' // fragment
                    all_valid = .false.
                end if
            end if
        end do
    end function verify_fragments

end program test_basic_function_inference
