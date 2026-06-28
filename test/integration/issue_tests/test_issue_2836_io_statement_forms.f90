program test_issue_2836_io_statement_forms
    ! End-to-end: the parser accepts valid READ/WRITE forms that previously
    ! failed. Keyword control items (unit=, fmt=), a format given as a
    ! character expression, and variables named read/write all parse and
    ! round-trip through the standardization pipeline.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_contains('examples/f90/issue_2836_io_keyword_unit_format.f90', &
        'read(10', 'keyword unit=/fmt= control items', all_passed)
    call check_contains('examples/f90/issue_2836_io_expression_format.f90', &
        'iostat=stat', 'expression format with iostat', all_passed)
    call check_contains('examples/f90/issue_2836_io_expression_format.f90', &
        '//fmt', 'expression format kept as expression', all_passed)
    call check_contains('examples/f90/issue_2836_io_keyword_variable_names.f90', &
        'read = ', 'read used as a variable name', all_passed)
    call check_contains('examples/f90/issue_2836_io_keyword_variable_names.f90', &
        'write = ', 'write used as a variable name', all_passed)

    if (all_passed) then
        write (*, '(a)') 'PASS: issue 2836 IO statement forms parse and round-trip'
    else
        stop 1
    end if

contains

    subroutine check_contains(path, needle, label, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: needle
        character(len=*), intent(in) :: label
        logical, intent(inout) :: passed
        type(transform_context_t) :: context
        character(len=:), allocatable :: source, transformed, error_msg

        call read_example(path, source)
        context%input_mode = INPUT_MODE_STANDARD
        call transform_with_context(source, transformed, error_msg, context)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(a)') 'FAIL: '//label//': '//trim(error_msg)
                passed = .false.
                return
            end if
        end if

        if (.not. allocated(transformed)) then
            write (error_unit, '(a)') 'FAIL: '//label//': no output produced'
            passed = .false.
            return
        end if

        if (index(transformed, needle) == 0) then
            write (error_unit, '(a)') 'FAIL: '//label// &
                ': expected substring "'//needle//'" missing from output'
            write (error_unit, '(a)') transformed
            passed = .false.
        end if
    end subroutine check_contains

    include '../../common/read_example.inc'
end program test_issue_2836_io_statement_forms
