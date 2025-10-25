program test_issue_1899_data_scalars
    use fortfront
    implicit none

    call check_scalar_list()
    call check_multiple_sets()
    print *, "PASSED"

contains

    subroutine check_scalar_list()
        character(len=*), parameter :: src = &
     &      "program test_data" // new_line('a') // &
     &      "    implicit none" // new_line('a') // &
     &      "    integer :: i, j, k" // new_line('a') // &
     &      "    real :: x, y" // new_line('a') // &
     &      "    data i, j, k / 1, 2, 3 /" // new_line('a') // &
     &      "    data x, y / 3.5, 7.2 /" // new_line('a') // &
     &      "    print *, i, j, k, x, y" // new_line('a') // &
     &      "end program test_data"
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: ok

        call transform_lazy_fortran_string(src, output, error_msg)

        ok = allocated(output)
        if (ok) ok = index(output, "i = 1") > 0
        if (ok) ok = index(output, "j = 2") > 0
        if (ok) ok = index(output, "k = 3") > 0
        if (ok) then
            ok = index(output, "x = 3.5") > 0 .or. &
                 index(output, "x = 3.50000000") > 0
        end if
        if (ok) then
            ok = index(output, "y = 7.2") > 0 .or. &
                 index(output, "y = 7.20000000") > 0
        end if
        if (ok) ok = .not. allocated(error_msg) .or. len_trim(error_msg) == 0

        if (.not. ok) then
            print *, "FAILED [scalar-list]"
            if (allocated(output)) then
                print *, trim(output)
            else
                print *, "OUTPUT missing"
            end if
            if (allocated(error_msg)) then
                if (len_trim(error_msg) > 0) print *, trim(error_msg)
            end if
            stop 1
        end if
    end subroutine check_scalar_list

    subroutine check_multiple_sets()
        character(len=*), parameter :: src = &
     &      "program data_sets" // new_line('a') // &
     &      "    implicit none" // new_line('a') // &
     &      "    integer :: a, b" // new_line('a') // &
     &      "    data a / 1 /, b / 2 /" // new_line('a') // &
     &      "    print *, a, b" // new_line('a') // &
     &      "end program data_sets"
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: ok

        call transform_lazy_fortran_string(src, output, error_msg)

        ok = allocated(output)
        if (ok) ok = index(output, "a = 1") > 0
        if (ok) ok = index(output, "b = 2") > 0
        if (ok) ok = .not. allocated(error_msg) .or. len_trim(error_msg) == 0

        if (.not. ok) then
            print *, "FAILED [multiple-sets]"
            if (allocated(output)) then
                print *, trim(output)
            else
                print *, "OUTPUT missing"
            end if
            if (allocated(error_msg)) then
                if (len_trim(error_msg) > 0) print *, trim(error_msg)
            end if
            stop 1
        end if
    end subroutine check_multiple_sets
end program test_issue_1899_data_scalars
