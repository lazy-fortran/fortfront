program test_monomorphization_subroutine
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
        cleanup_temp_directory, join_path, &
        path_separator_for
    use test_shell_commands, only: build_compile_command
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    character(len=*), parameter :: tmp_file = 'fortfront_mono_subroutine.f90'
    integer :: exit_code, unit
    integer :: module_pos, program_pos
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    character(len=:), allocatable :: tmp_path
    character(len=:), allocatable :: compile_cmd

    call read_example('examples/lf/monomorphization_scale_subroutine.lf', input)

    call transform_lazy_fortran_string(input, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A,A)') 'subroutine monomorphization error: ', &
            trim(error_msg)
        error stop 1
    end if

    write (error_unit, '(A)') '=== GENERATED OUTPUT ==='
    write (error_unit, '(A)') trim(output)
    write (error_unit, '(A)') '=== END OUTPUT ==='

    call assert_contains(output, 'scale__i32_i32', &
        'missing integer specialization for scale')
    call assert_contains(output, 'scale__r64_r64', &
        'missing real specialization for scale')
    call assert_contains(output, 'interface scale', &
        'missing generic interface for scale')
    call assert_contains(output, '    use auto_scale', &
        'program did not import generated module')

    module_pos = index(output, 'module auto_scale')
    program_pos = index(output, 'program main')
    if (module_pos <= 0 .or. program_pos <= 0 .or. module_pos > program_pos) then
        write (error_unit, '(A)') 'module auto_scale must precede program main'
        error stop 1
    end if

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
        write (error_unit, '(A)') 'gfortran rejected subroutine specialization output'
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


end program test_monomorphization_subroutine
