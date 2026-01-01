program test_issue_2014_elemental_only
    use transformation_api, only: compile_source, compilation_options_t
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
                                       cleanup_temp_directory, join_path, &
                                       path_separator_for
    implicit none

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit, io_status
    logical :: found_elemental_integer
    character(len=256) :: line
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    integer :: exit_code

    found_elemental_integer = .false.

    exit_code = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    input_file = join_path(temp_dir, 'test_issue_2014_elemental_only_input.f90', sep)
    output_file = join_path(temp_dir, 'test_issue_2014_elemental_only_output.f90', &
                            sep)

    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'program test_elemental_function'
    write (unit, '(a)') '    implicit none'
    write (unit, '(a)') '    integer, dimension(5) :: arr'
    write (unit, '(a)') '    integer :: i'
    write (unit, '(a)') '    arr = [(i, i = 1, 5)]'
    write (unit, '(a)') 'contains'
    write (unit, '(a)') '    elemental integer function double(x)'
    write (unit, '(a)') '        integer, intent(in) :: x'
    write (unit, '(a)') '        double = x * 2'
    write (unit, '(a)') '    end function double'
    write (unit, '(a)') 'end program test_elemental_function'
    close (unit)

    options%output_file = output_file

    call compile_source(input_file, options, error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, 'Compiler reported error: ', trim(error_msg)
        exit_code = 1
        goto 999
    end if

    open (newunit=unit, file=output_file, status='old', action='read')
    do
        read (unit, '(a)', iostat=io_status) line
        if (io_status /= 0) exit
        if (.not. found_elemental_integer) then
            if (index(line, 'function double') > 0 .and. &
                index(line, 'elemental') > 0 .and. &
                index(line, 'integer') > 0) then
                found_elemental_integer = .true.
            end if
        end if
        if (found_elemental_integer) exit
    end do
    close (unit)

    if (.not. found_elemental_integer) then
        print *, 'FAIL: elemental integer function keywords missing in output'
        exit_code = 1
        goto 999
    end if

    print *, 'PASS: ELEMENTAL-only function prefix preserved'
999 continue
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code
end program test_issue_2014_elemental_only
