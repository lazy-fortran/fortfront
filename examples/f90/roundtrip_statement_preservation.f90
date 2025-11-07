! Comprehensive test for Statement Preservation round-trip
! Tests: all statements in correct order, loop bodies complete, IF block bodies complete,
!        statements after control flow constructs, no silent deletion of any statements

program roundtrip_statement_preservation
    implicit none

    integer :: i, j, n
    real :: arr(10), sum_val, product_val
    logical :: flag
    character(len=20) :: status

    n = 10
    sum_val = 0.0
    product_val = 1.0
    flag = .true.

    ! Statement before loop
    print *, 'Starting computation with n =', n

    ! DO loop with multiple statements in body
    do i = 1, n
        arr(i) = real(i)
        sum_val = sum_val + arr(i)
        product_val = product_val * arr(i)
    end do

    ! Statement after loop - must be preserved
    print *, 'Loop completed, sum =', sum_val

    ! Nested IF blocks with statements in all branches
    if (sum_val > 50.0) then
        status = 'high'
        print *, 'Sum is high'
        sum_val = sum_val / 2.0
    else if (sum_val > 20.0) then
        status = 'medium'
        print *, 'Sum is medium'
        sum_val = sum_val * 1.5
    else
        status = 'low'
        print *, 'Sum is low'
        sum_val = sum_val * 2.0
    end if

    ! Statement after IF block - must be preserved
    print *, 'Status:', trim(status)

    ! Nested loops with statements between and after
    do i = 1, 3
        print *, 'Outer loop iteration:', i
        do j = 1, 3
            arr(i) = arr(i) + real(j)
            print *, '  Inner loop:', i, j
        end do
        print *, 'Completed inner loop for i =', i
    end do

    ! Statement after nested loops - must be preserved
    print *, 'All loops completed'

    ! SELECT CASE with statements in all cases
    select case (n)
    case (1:5)
        status = 'small'
        flag = .false.
    case (6:10)
        status = 'medium'
        flag = .true.
    case (11:)
        status = 'large'
        flag = .true.
    case default
        status = 'unknown'
        flag = .false.
    end select

    ! Statement after SELECT CASE - must be preserved
    print *, 'Case status:', trim(status), flag

    ! WHERE construct with statements before and after
    print *, 'Applying WHERE construct'
    where (arr > 5.0)
        arr = arr * 2.0
    elsewhere
        arr = arr + 1.0
    end where
    print *, 'WHERE construct completed'

    ! Multiple statements at end - all must be preserved
    print *, 'Final array values:', arr(1:5)
    print *, 'Final sum:', sum_val
    print *, 'Final product:', product_val
    print *, 'Program ending'

contains

    ! Subroutine with statements before, in, and after control flow
    subroutine process_data(data, n, threshold)
        integer, intent(in) :: n
        real, intent(in) :: threshold
        real, dimension(n), intent(inout) :: data
        integer :: i
        real :: temp

        ! Statement before loop
        temp = 0.0

        ! Loop with complete body
        do i = 1, n
            if (data(i) > threshold) then
                data(i) = data(i) * 2.0
                temp = temp + data(i)
            else
                data(i) = data(i) + 1.0
            end if
        end do

        ! Statement after loop - must not be deleted
        print *, 'Processed', n, 'elements, sum =', temp

    end subroutine process_data

    ! Function with multiple return points - all statements preserved
    function check_range(value, min_val, max_val) result(in_range)
        real, intent(in) :: value, min_val, max_val
        logical :: in_range

        ! Early return case
        if (value < min_val) then
            in_range = .false.
            return
        end if

        ! Another early return
        if (value > max_val) then
            in_range = .false.
            return
        end if

        ! Default case
        in_range = .true.

    end function check_range

end program roundtrip_statement_preservation
