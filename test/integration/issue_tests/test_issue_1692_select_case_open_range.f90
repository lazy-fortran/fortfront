program test_issue_1692_select_case_open_range
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg

    print *, '=== Issue #1692: SELECT CASE with open-ended ranges ==='

    call read_example('examples/f90/issue_1692_select_case_open_range.f90', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    call require(.not. allocated(error_msg) .or. len_trim(error_msg) == 0, &
                 'Unexpected parser error: '//merge(error_msg, '', allocated(error_msg)))
    call require(allocated(output_code), 'No output generated')

    call require(index(output_code, 'select case') > 0, &
                 'SELECT CASE block missing (entire construct removed)')
    call require(index(output_code, 'case (0:50)') > 0, 'Missing case (0:50) closed range')
    call require(index(output_code, 'case (51:70)') > 0, 'Missing case (51:70) closed range')
    call require(index(output_code, 'case (71:85)') > 0, 'Missing case (71:85) closed range')
    call require(index(output_code, 'case (86:95)') > 0, 'Missing case (86:95) closed range')
    call require(index(output_code, 'case (96:)') > 0, 'Missing case (96:) open-ended range')
    call require(index(output_code, 'case default') > 0, 'Missing default case block')
    call require(index(output_code, "category = 'F'") > 0, 'Missing first case body')
    call require(index(output_code, "category = 'A'") > 0, 'Missing open range case body')
    call require(index(output_code, 'end select') > 0, 'Missing end select')

    print *, 'PASS: Open-ended ranges in SELECT CASE preserved correctly'

contains

    include '../../common/read_example.inc'

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


end program test_issue_1692_select_case_open_range
