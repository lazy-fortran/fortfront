program test_issue_1739_program_prefixes
    use transformation_api, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit, io_status
    logical :: found_elemental_pure
    logical :: found_impure_elemental
    logical :: found_recursive
    logical :: found_nonrecursive
    character(len=256) :: line
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    integer :: exit_code

    found_elemental_pure = .false.
    found_impure_elemental = .false.
    found_recursive = .false.
    found_nonrecursive = .false.

    exit_code = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    input_file = join_path(temp_dir, 'test_issue_1739_program_prefixes_input.f90', &
        sep)
    output_file = join_path(temp_dir, &
        'test_issue_1739_program_prefixes_output.f90', sep)

    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'program test_issue_1739'
    write (unit, '(a)') '    implicit none'
    write (unit, '(a)') '    integer :: seed'
    write (unit, '(a)') '    seed = 1'
    write (unit, '(a)') 'contains'
    write (unit, '(a)') '    elemental pure function square(x) result(y)'
    write (unit, '(a)') '        real, intent(in) :: x'
    write (unit, '(a)') '        real :: y'
    write (unit, '(a)') '        y = x * x'
    write (unit, '(a)') '    end function square'
    write (unit, '(a)') ''
    write (unit, '(a)') '    impure elemental subroutine scale(x)'
    write (unit, '(a)') '        real, intent(inout) :: x'
    write (unit, '(a)') '        x = 2.0 * x'
    write (unit, '(a)') '    end subroutine scale'
    write (unit, '(a)') ''
    write (unit, '(a)') '    recursive function factorial(n) result(val)'
    write (unit, '(a)') '        integer, intent(in) :: n'
    write (unit, '(a)') '        integer :: val'
    write (unit, '(a)') '        if (n <= 1) then'
    write (unit, '(a)') '            val = 1'
    write (unit, '(a)') '        else'
    write (unit, '(a)') '            val = n * factorial(n - 1)'
    write (unit, '(a)') '        end if'
    write (unit, '(a)') '    end function factorial'
    write (unit, '(a)') ''
    write (unit, '(a)') '    nonrecursive function identity(v) result(res)'
    write (unit, '(a)') '        integer, intent(in) :: v'
    write (unit, '(a)') '        integer :: res'
    write (unit, '(a)') '        res = v'
    write (unit, '(a)') '    end function identity'
    write (unit, '(a)') 'end program test_issue_1739'
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
        if (.not. found_elemental_pure) then
            if (index(line, 'function square') > 0 .and. &
                index(line, 'elemental') > 0 .and. &
                index(line, 'pure') > 0) then
                found_elemental_pure = .true.
            end if
        end if
        if (.not. found_impure_elemental) then
            if (index(line, 'subroutine scale') > 0 .and. &
                index(line, 'elemental') > 0 .and. &
                index(line, 'impure') > 0) then
                found_impure_elemental = .true.
            end if
        end if
        if (.not. found_recursive) then
            if (index(line, 'function factorial') > 0 .and. &
                index(line, 'recursive') > 0) then
                found_recursive = .true.
            end if
        end if
        if (.not. found_nonrecursive) then
            if (index(line, 'function identity') > 0 .and. &
                index(line, 'nonrecursive') > 0) then
                found_nonrecursive = .true.
            end if
        end if
        if (found_elemental_pure .and. found_impure_elemental .and. &
            found_recursive .and. found_nonrecursive) exit
    end do
    close (unit)

    if (.not. found_elemental_pure) then
        print *, 'FAIL: elemental pure function keywords missing in output'
        exit_code = 1
        goto 999
    end if

    if (.not. found_impure_elemental) then
        print *, 'FAIL: impure elemental subroutine keywords missing in output'
        exit_code = 1
        goto 999
    end if

    if (.not. found_recursive) then
        print *, 'FAIL: recursive function keyword missing in output'
        exit_code = 1
        goto 999
    end if

    if (.not. found_nonrecursive) then
        print *, 'FAIL: nonrecursive function keyword missing in output'
        exit_code = 1
        goto 999
    end if

    print *, 'PASS: Program-contained prefixes preserved'
    999 continue
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code

contains

    include '../common/filesystem_helpers.inc'

end program test_issue_1739_program_prefixes
