program test_monomorphization_three_types
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
        cleanup_temp_directory, join_path, &
        path_separator_for
    use test_shell_commands, only: build_compile_command
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    character(len=*), parameter :: tmp_file = 'fortfront_mono_three.f90'
    integer :: exit_code, unit
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    character(len=:), allocatable :: tmp_path
    character(len=:), allocatable :: compile_cmd

    call read_example('examples/lf/monomorphization_add_three_types.lf', input)

    call transform_lazy_fortran_string(input, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'three_types: unexpected error ' // trim(error_msg)
        error stop 1
    end if

    call assert_contains(output, 'add__i32_i32', &
        'missing integer specialization in three type test')
    call assert_contains(output, 'add__r64_r64', &
        'missing real specialization in three type test')
    call assert_contains(output, 'add__c64_c64', &
        'missing complex specialization in three type test')

    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) error stop 'FAIL: could not create temporary directory'
    sep = path_separator_for(temp_dir)
    tmp_path = join_path(temp_dir, tmp_file, sep)

    open (newunit=unit, file=tmp_path, status='replace', action='write')
    write (unit, '(A)') trim(output)
    close (unit)

    compile_cmd = build_compile_command(tmp_path, '', temp_dir, is_windows)
    call execute_command_line(compile_cmd, exitstat=exit_code, wait=.true.)
    call cleanup_temp_directory(temp_dir, is_windows)
    if (exit_code /= 0) then
        write (error_unit, '(A)') 'gfortran rejected multi-type output'
        error stop 1
    end if

contains

    include '../../common/read_example.inc'

    subroutine assert_contains(text, token, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: token
        character(len=*), intent(in) :: message

        if (index(text, token) <= 0) then
            write (error_unit, '(A)') trim(message)
            error stop 1
        end if
    end subroutine assert_contains


end program test_monomorphization_three_types
