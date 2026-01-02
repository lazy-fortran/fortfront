program test_issue_1349_select_case
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg

    print *, '=== Issue #1349: SELECT CASE bodies preserved ==='

    call read_example('examples/f90/issue_1349_select_case_bodies.f90', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    call require(.not. allocated(error_msg) .or. len_trim(error_msg) == 0, &
                 'Unexpected parser error: '//merge(error_msg, '', allocated(error_msg)))
    call require(allocated(output_code), 'No output generated')

    call require(index(output_code, 'case (1)') > 0, 'Missing case (1) block')
    call require(index(output_code, 'case (2:3)') > 0, 'Missing case (2:3) block')
    call require(index(output_code, 'case (4)') > 0, 'Missing case (4) block')
    call require(index(output_code, 'case (5, 6, 7)') > 0, 'Missing multi-value case block')
    call require(index(output_code, 'case default') > 0, 'Missing default case block')
    call require(index(output_code, 'print *, ''One''') > 0, 'Missing first case body')
    call require(index(output_code, 'print *, ''Two or Three''') > 0, &
                 'Missing range case body')
    call require(index(output_code, 'print *, ''Four''') > 0, 'Missing third case body')
    call require(index(output_code, 'print *, ''FiveSixSeven''') > 0, 'Missing multi-value case body')
    call require(index(output_code, 'print *, ''Other''') > 0, 'Missing default body')
    call require(index(output_code, 'select case (i)'//new_line('a')// &
                       '        end select') == 0, &
                 'Case bodies emitted outside of SELECT construct')

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine require(cond, message)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: message
        if (.not. cond) then
            if (len_trim(message) > 0) then
                write (error_unit, '(a)') 'ERROR: ' // trim(message)
            end if
            stop 1
        end if
    end subroutine require


end program test_issue_1349_select_case
