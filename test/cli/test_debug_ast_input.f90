program test_debug_ast_input
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_tooling, only: read_file_contents
    implicit none

    character(len=512) :: executable
    character(len=256) :: input_path, output_path, error_path
    character(len=:), allocatable :: output, message
    integer :: unit, status
    logical :: found

    if (.not. should_run_system_tests()) then
        print *, 'SKIP: debug_ast input test (set RUN_SYSTEM_TESTS=1 to enable)'
        stop
    end if
    call find_executable(executable, found, 'debug_ast')
    if (.not. found) error stop 'debug_ast executable missing'
    call make_tmpfile(input_path)
    call make_tmpfile(output_path)
    call make_tmpfile(error_path)

    call check_source('alpha', 'beta')
    call check_source('beta', 'alpha')
    open (newunit=unit, file=trim(input_path), status='replace')
    write (unit, '(A)') 'program invalid'
    write (unit, '(A)') 'x=(.false. ? 7 : 9)'
    write (unit, '(A)') 'end program invalid'
    close (unit)
    call execute_command_line('"'//trim(executable)//'" "'//trim(input_path)// &
                          '" > "'//trim(output_path)//'" 2> "'//trim(error_path)//'"', &
                              exitstat=status)
    if (status == 0) error stop 'debug_ast accepted unsupported conditional syntax'
    call read_file_contents(trim(output_path), output, message)
    if (len_trim(output) /= 0) error stop 'debug_ast emitted JSON for rejected input'
    call read_file_contents(trim(error_path), output, message)
    if (index(output, 'not supported') == 0) &
        error stop 'debug_ast did not report the parse failure on stderr'

    call delete_file(trim(input_path))
    call delete_file(trim(output_path))
    call delete_file(trim(error_path))
    print *, 'PASS: debug_ast inspects supplied source and rejects invalid input'

contains

    include '../common/cli_system_tests.inc'

    subroutine check_source(name, absent_name)
        character(len=*), intent(in) :: name, absent_name

        open (newunit=unit, file=trim(input_path), status='replace')
        write (unit, '(A)') 'program '//name
        write (unit, '(A)') 'print *, 42'
        write (unit, '(A)') 'end program '//name
        close (unit)
        call execute_command_line('"'//trim(executable)//'" "'//trim(input_path)// &
                          '" > "'//trim(output_path)//'" 2> "'//trim(error_path)//'"', &
                                  exitstat=status)
        if (status /= 0) error stop 'debug_ast rejected valid source'
        call read_file_contents(trim(output_path), output, message)
        if (index(output, name) == 0) error stop 'debug_ast lost supplied program'
        if (index(output, absent_name) /= 0) error stop 'debug_ast dumped another input'
    end subroutine check_source
end program test_debug_ast_input
