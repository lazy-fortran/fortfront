program test_issue_2252_data_value_implied_do
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    call check_value_implied_do()
    print *, "PASSED"

contains

    include '../common/read_example.inc'

    subroutine check_value_implied_do()
        character(len=:), allocatable :: src
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: ok

        call read_example('examples/f90/issue_2252_data_value_implied_do.f90', src)
        call transform_lazy_fortran_string(src, output, error_msg)

        ok = allocated(output)
        if (ok) ok = index(output, "data coeff/(i*1.0, i = 1, 2) /") > 0 .or. &
            index(output, "data coeff/(i*1.0_8, i = 1, 2) /") > 0
        if (ok) ok = .not. allocated(error_msg) .or. len_trim(error_msg) == 0

        if (.not. ok) then
            print *, "FAILED [value-implied-do]"
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
    end subroutine check_value_implied_do

end program test_issue_2252_data_value_implied_do
