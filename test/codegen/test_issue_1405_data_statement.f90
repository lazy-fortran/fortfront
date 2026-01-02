program test_issue_1405_data_statement
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    print *, "=== Codegen: DATA statements preserved ==="

    call check_case("implicit-array-init", &
                    'examples/f90/data_statement_array_literal.f90', &
                    'integer, dimension(3) :: common_array', &
                    'data common_array/1, 2, 3 /', &
                    '', &
                    'integer :: common_array(3)')

    print *, "PASSED"

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine check_case(name, source_path, expect_decl, expect_assign, forbidden, &
                          alt_decl)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source_path
        character(len=*), intent(in) :: expect_decl
        character(len=*), intent(in) :: expect_assign
        character(len=*), intent(in) :: forbidden
        character(len=*), intent(in), optional :: alt_decl
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: success

        call read_example(source_path, source)
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


end program test_issue_1405_data_statement
