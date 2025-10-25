program test_issue_1899_data_multi_objects
    use fortfront
    implicit none

    print *, "=== Codegen: DATA statements with multiple objects ==="

    call check_case("multi-object scalars", &
        'program demo'//new_line('a')// &
        '    implicit none'//new_line('a')// &
        '    integer :: i, j, k'//new_line('a')// &
        '    real :: x, y'//new_line('a')// &
        '    data i, j, k / 1, 2, 3 /'//new_line('a')// &
        '    data x, y / 3.5, 7.2 /'//new_line('a')// &
        'end program demo', &
        [ character(len=16) :: &
            'i = 1', &
            'j = 2', &
            'k = 3', &
            'x = 3.5', &
            'y = 7.2' ])

    print *, "PASSED"

contains

    subroutine check_case(name, source, expected)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        character(len=*), dimension(:), intent(in) :: expected
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: success
        integer :: i

        call transform_lazy_fortran_string(source, output, error_msg)

        success = .true.
        if (.not. allocated(output)) success = .false.
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) success = .false.
        end if

        if (success) then
            do i = 1, size(expected)
                if (len_trim(expected(i)) == 0) cycle
                if (index(output, trim(expected(i))) == 0) then
                    success = .false.
                    exit
                end if
            end do
        end if

        if (.not. success) then
            print *, "FAILED [", trim(name), "]"
            if (allocated(output)) then
                print *, "OUTPUT:"
                print *, trim(output)
            else
                print *, "OUTPUT missing"
            end if
            if (allocated(error_msg)) then
                if (len_trim(error_msg) > 0) then
                    print *, "ERRORS:"
                    print *, trim(error_msg)
                end if
            end if
            stop 1
        end if
    end subroutine check_case

end program test_issue_1899_data_multi_objects
