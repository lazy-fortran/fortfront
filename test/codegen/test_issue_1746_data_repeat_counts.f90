program test_issue_1746_data_repeat_counts
    use fortfront
    implicit none

    print *, "=== Codegen: DATA statements with repeat counts ==="

    call check_case("simple-repeat", &
                    'integer :: arr(5)'//new_line('a')// &
                    'DATA arr /5*0/'//new_line('a')// &
                    'print *, arr(1)', &
                    'integer :: arr(5)', &
                    '(/0, 0, 0, 0, 0 /)', &
                    '')

    call check_case("mixed-repeat-and-values", &
                    'integer :: arr(7)'//new_line('a')// &
                    'DATA arr /3*1, 2, 3*0/'//new_line('a')// &
                    'print *, arr(1)', &
                    'integer :: arr(7)', &
                    '(/1, 1, 1, 2, 0, 0, 0 /)', &
                    '')

    call check_case("repeat-count-kind", &
                    'integer :: arr(5)'//new_line('a')// &
                    'DATA arr /5_1*0/'//new_line('a')// &
                    'print *, arr(1)', &
                    'integer :: arr(5)', &
                    '(/0, 0, 0, 0, 0 /)', &
                    '5_1*0')

    print *, "PASSED"

contains

    subroutine check_case(name, source, expect_decl, expect_assign, forbidden, alt_decl)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expect_decl
        character(len=*), intent(in) :: expect_assign
        character(len=*), intent(in) :: forbidden
        character(len=*), intent(in), optional :: alt_decl
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: success

        call transform_lazy_fortran_string(source, output, error_msg)

        success = .true.
        if (.not. allocated(output)) success = .false.
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) success = .false.
        end if

        if (success) then
            if (index(output, expect_decl) == 0) then
                if (.not. (present(alt_decl) .and. index(output, alt_decl) > 0)) then
                    success = .false.
                end if
            end if
        end if

        if (success) then
            if (index(output, expect_assign) == 0) success = .false.
        end if

        if (success .and. len_trim(forbidden) > 0) then
            if (index(output, forbidden) > 0) success = .false.
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

end program test_issue_1746_data_repeat_counts
