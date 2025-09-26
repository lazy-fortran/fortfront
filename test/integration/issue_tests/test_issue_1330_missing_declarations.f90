program test_issue_1330_missing_declarations
    use frontend, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call test_missing_variable_declarations()
    print *, 'Issue #1330 regression test passed.'

contains

    subroutine test_missing_variable_declarations()
        logical :: has_real_kind

        input = 'real(kind=8) :: x, y' // new_line('a') // &
                'n = 1000000' // new_line('a') // &
                'count = 0' // new_line('a') // &
                'do i = 1, n' // new_line('a') // &
                '    call random_number(x)' // new_line('a') // &
                '    call random_number(y)' // new_line('a') // &
                '    if (x*x + y*y <= 1.0) count = count + 1' // new_line('a') // &
                'end do' // new_line('a') // &
                'pi_estimate = 4.0 * real(count) / real(n)' // new_line('a') // &
                'print *, "Number of points:", n' // new_line('a') // &
                'print *, "Estimated value of pi:", pi_estimate'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: transform error:', trim(error_msg)
                error stop 1
            end if
        end if

        call assert_contains(output, 'integer :: n', 'missing integer declaration for n')
        call assert_contains(output, 'integer :: count', 'missing integer declaration for count')
        call assert_contains(output, 'integer :: i', 'missing integer declaration for i')

        has_real_kind = index(output, 'real :: pi_estimate') > 0
        if (.not. has_real_kind) then
            has_real_kind = index(output, 'real(8) :: pi_estimate') > 0
        end if
        if (.not. has_real_kind) then
            print *, 'FAIL: missing real declaration for pi_estimate'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
    end subroutine test_missing_variable_declarations

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) == 0) then
            print *, 'FAIL:', trim(message)
            print *, 'Pattern:', trim(pattern)
            print *, 'Output:'
            print *, trim(text)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_1330_missing_declarations
