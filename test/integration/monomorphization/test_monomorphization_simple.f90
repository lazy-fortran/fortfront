program test_monomorphization_simple
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
        cleanup_temp_directory, join_path, &
        path_separator_for
    use test_shell_commands, only: build_compile_command
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    character(len=*), parameter :: tmp_file = 'fortfront_mono_simple.f90'
    integer :: module_pos, program_pos
    integer :: exit_code, unit
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    character(len=:), allocatable :: tmp_path
    character(len=:), allocatable :: compile_cmd

    input = 'function add(a, b)' // new_line('A') // &
        '    add = a + b' // new_line('A') // &
        'end function' // new_line('A') // &
        '' // new_line('A') // &
        'x = add(5, 3)' // new_line('A') // &
        'y = add(2.5d0, 1.5d0)'

    call transform_lazy_fortran_string(input, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'monomorphization_simple: unexpected error ' // trim(error_msg)
        error stop 1
    end if

    if (index(output, 'add__i32_i32') <= 0) then
        write (error_unit, '(A)') 'missing integer specialization add__i32_i32'
        error stop 1
    end if

    if (index(output, 'add__r64_r64') <= 0) then
        write (error_unit, '(A)') 'missing real specialization add__r64_r64'
        error stop 1
    end if

    if (index(output, 'interface add') <= 0) then
        write (error_unit, '(A)') 'missing generic interface for add'
        error stop 1
    end if

    if (index(output, '    use auto_add') <= 0) then
        write (error_unit, '(A)') 'program did not import generated module'
        error stop 1
    end if

    module_pos = index(output, 'module auto_add')
    program_pos = index(output, 'program main')
    if (module_pos <= 0 .or. program_pos <= 0 .or. module_pos > program_pos) then
        write (error_unit, '(A)') 'module auto_add must precede program main'
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
        write (error_unit, '(A)') 'gfortran rejected generated code'
        error stop 1
    end if
end program test_monomorphization_simple
