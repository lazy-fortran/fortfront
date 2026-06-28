program test_issue_1899_data_multi_objects
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    print *, "=== Codegen: DATA statements with multiple objects ==="

    call check_case("multi-object scalars", &
        'examples/f90/data_multi_objects.f90', &
        [ character(len=32) :: &
        'data i, j, k/1, 2, 3 /', &
        'data x, y/3.5_8, 7.2_8 /' ])

    print *, "PASSED"

contains

    include '../common/read_example.inc'

    subroutine check_case(name, source_path, expected)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source_path
        character(len=*), dimension(:), intent(in) :: expected
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: success
        integer :: i

        call read_example(source_path, source)
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
