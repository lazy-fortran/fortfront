program test_monomorphization_simple
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    character(len=*), parameter :: tmp_file = 'fortfront_mono_simple.f90'
    character(len=*), parameter :: compile_cmd = 'gfortran -fsyntax-only '
    integer :: module_pos, program_pos
    integer :: exit_code, unit

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

    open (newunit=unit, file=tmp_file, status='replace', action='write')
    write (unit, '(A)') trim(output)
    close (unit)

    call execute_command_line(compile_cmd // tmp_file, exitstat=exit_code, &
                              wait=.true.)
    if (exit_code /= 0) then
        write (error_unit, '(A)') 'gfortran rejected generated code'
        call execute_command_line('rm -f ' // tmp_file, wait=.true.)
        error stop 1
    end if

    call execute_command_line('rm -f ' // tmp_file, wait=.true.)
end program test_monomorphization_simple
