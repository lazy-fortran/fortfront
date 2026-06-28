program test_issue_2024_char_function_duplicate_and_missing
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=*), parameter :: example_path = 'examples/f90/' // &
        'issue_2024_char_function_duplicate_and_missing.f90'
    character(len=:), allocatable :: source, output, error_msg
    character(len=512) :: line_buffer
    integer :: unit, ios
    logical :: first_line

    source = ''
    first_line = .true.
    open (newunit=unit, file=example_path, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: could not open ' // example_path
        stop 1
    end if

    do
        read (unit, '(A)', iostat=ios) line_buffer
        if (ios /= 0) exit
        if (first_line) then
            source = trim(line_buffer)
            first_line = .false.
        else
            source = source // new_line('a') // trim(line_buffer)
        end if
    end do
    close (unit)

    if (first_line) then
        print *, 'FAIL: example file was empty'
        stop 1
    end if

    source = source // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    if (.not. allocated(output)) then
        print *, 'FAIL: transformation produced no output'
        stop 1
    end if

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: transformation error:', trim(error_msg)
            stop 1
        end if
    end if

    ! Verify no duplicate declaration of 'i'
    call assert_no_duplicate_declaration(output, 'i')

    ! Verify 'code' is properly declared
    call assert_contains(output, 'integer :: i, code')

    ! Verify output compiles with gfortran
    call test_compilation(output, 'test_issue_2024_output')

    print *, &
        'PASS: Issue #2024 - Character function with RESULT preserves declarations'

contains

    subroutine assert_no_duplicate_declaration(text, var_name)
        character(len=*), intent(in) :: text, var_name
        integer :: count, pos
        character(len=:), allocatable :: pattern

        pattern = 'integer :: ' // trim(var_name)
        count = 0
        pos = 1

        do while (pos > 0)
            pos = index(text(pos:), pattern)
            if (pos > 0) then
                count = count + 1
                pos = pos + len(pattern)
            end if
        end do

        if (count > 1) then
            print *, 'ERROR: Variable ' // trim(var_name) // ' declared', &
                count, 'times'
            error stop 'Duplicate declaration detected'
        end if
    end subroutine assert_no_duplicate_declaration

    subroutine assert_contains(text, substring)
        character(len=*), intent(in) :: text, substring

        if (index(text, substring) == 0) then
            print *, 'ERROR: Expected substring not found: ' // trim(substring)
            error stop 'Assertion failed'
        end if
    end subroutine assert_contains

    subroutine test_compilation(source_text, basename)
        use test_shell_commands, only: build_compile_command
        character(len=*), intent(in) :: source_text, basename
        character(len=256) :: filename, cmd, temp_dir, obj_file
        integer :: unit, ios, exit_code
        logical :: is_windows

        is_windows = check_if_windows()

        if (is_windows) then
            call get_environment_variable('TEMP', temp_dir, status=ios)
            if (ios /= 0) temp_dir = '.'
        else
            temp_dir = '/tmp'
        end if

        filename = trim(temp_dir) // '/' // trim(basename) // '.f90'
        obj_file = trim(temp_dir) // '/' // trim(basename) // '.o'

        open (newunit=unit, file=filename, status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) error stop 'Failed to create temporary file'

        write (unit, '(A)') source_text
        close (unit)

        cmd = build_compile_command(trim(filename), '', trim(temp_dir), is_windows)
        call execute_command_line(cmd, exitstat=exit_code)

        if (exit_code /= 0) then
            print *, 'ERROR: Generated code does not compile'
            error stop 'Compilation test failed'
        end if
    end subroutine test_compilation

    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat

        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')

        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows

end program test_issue_2024_char_function_duplicate_and_missing
