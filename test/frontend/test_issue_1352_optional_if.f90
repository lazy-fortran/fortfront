program test_issue_1352_optional_if
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    call test_optional_program_contains()
    call test_optional_parameter_if_control()
    print *, ''
    print *, 'All tests passed for issue 1352.'

contains

    subroutine test_optional_program_contains()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_program, idx_contains, idx_subroutine

        call read_example('examples/f90/issue_1352_optional_subroutine_program.f90', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') &
                'FAIL: unexpected error for optional interface sample:'
            write (error_unit, '(a)') trim(error_msg)
            error stop 1
        end if

        idx_program = index(output_code, 'program test_optional')
        idx_contains = index(output_code, 'contains')
        idx_subroutine = index(output_code, 'subroutine greet')

        if (idx_program <= 0) then
            write (error_unit, '(a)') 'FAIL: program test_optional missing from output'
            error stop 1
        end if

        if (idx_contains <= idx_program) then
            write (error_unit, '(a)') &
                'FAIL: contains block not inserted after program header'
            error stop 1
        end if

        if (idx_subroutine <= idx_contains) then
            write (error_unit, '(a)') &
                'FAIL: greet subroutine not placed inside contains block'
            error stop 1
        end if

        if (index(output_code, 'optional :: title') <= 0) then
            write (error_unit, '(a)') &
                'FAIL: optional attribute lost when restructuring program'
            error stop 1
        end if

        print *, 'PASS: Optional subroutine moved inside contains block'
    end subroutine test_optional_program_contains

    subroutine test_optional_parameter_if_control()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_if, idx_then_stmt, idx_else, idx_else_stmt, idx_end_if

        call read_example('examples/f90/issue_1352_optional_module_if_control.f90', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error for issue 1352 sample:'
            write (error_unit, '(a)') trim(error_msg)
            error stop 1
        end if

        if (index(output_code, 'character(len=*), intent(in) :: name') <= 0) then
            write (error_unit, '(a)') 'FAIL: name parameter lost assumed length'
            error stop 1
        end if

        if (index(output_code, &
                  'character(len=*), intent(in), optional :: title') <= 0) then
            write (error_unit, '(a)') &
                'FAIL: title parameter lost optional attribute or length'
            error stop 1
        end if

        idx_if = index(output_code, 'if (present(title)) then')
        idx_then_stmt = index(output_code, "print *, title, ' ', name")
        idx_else = index(output_code, 'else')
        idx_else_stmt = index(output_code, 'print *, name')
        idx_end_if = index(output_code, 'end if')

        if (idx_if <= 0 .or. idx_then_stmt <= 0 .or. idx_else <= 0 .or. &
            idx_else_stmt <= &
            0 .or. idx_end_if <= 0) then
            write (error_unit, '(a)') &
                'FAIL: expected IF/ELSE structure not found in output'
            error stop 1
        end if

        if (.not. (idx_if < idx_then_stmt .and. idx_then_stmt < idx_else .and. &
                   idx_else < idx_else_stmt .and. idx_else_stmt < idx_end_if)) then
            write (error_unit, '(a)') 'FAIL: IF body or ELSE body emitted in wrong order'
            error stop 1
        end if

        print *, 'PASS: Optional parameter IF control preserved'
    end subroutine test_optional_parameter_if_control

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to read example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_1352_optional_if
