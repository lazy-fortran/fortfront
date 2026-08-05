program test_nested_procedure_constructs
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    options = compiler_frontend_options_t()
    call compile_frontend_from_string(source_text(), result, options)
    if (.not. result%success()) then
        write (error_unit, '(a)') 'FAIL: nested procedure constructs rejected: '// &
            trim(result%diagnostic_text)
        error stop 1
    end if

    print '(a)', 'PASS: nested procedure constructs parsed'

contains

    function source_text() result(source)
        character(len=:), allocatable :: source

        source = 'module nested_procedure_constructs'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine check(value)'//new_line('a')// &
            '    integer, intent(in) :: value'//new_line('a')// &
            '    integer :: i'//new_line('a')// &
            '    type :: holder'//new_line('a')// &
            '        class(*), allocatable :: node'//new_line('a')// &
            '    end type holder'//new_line('a')// &
            '    type(holder) :: arena'//new_line('a')// &
            '    type(holder), allocatable :: entries(:)'//new_line('a')// &
            '    class(*), allocatable :: obj'//new_line('a')// &
            '    character(len=:), allocatable :: current_type'//new_line('a')// &
            '    allocate (integer :: obj)'//new_line('a')// &
            '    allocate (entries(1))'//new_line('a')// &
            '    select type (node => entries(1)%node)'//new_line('a')// &
            '        type is (integer)'//new_line('a')// &
            '        if (value < 0 .and. &'//new_line('a')// &
            '            allocated(entries)) then'//new_line('a')// &
            '            if (value == -1) then'//new_line('a')// &
            '                return'//new_line('a')// &
            '            end if'//new_line('a')// &
            '        end if'//new_line('a')// &
            '        type_names: block'//new_line('a')// &
            '            integer :: block_value'//new_line('a')// &
            '            block_value = 1'//new_line('a')// &
            '            value = value'//new_line('a')// &
            '        end block type_names'//new_line('a')// &
            '        type is (holder)'//new_line('a')// &
            '        if (value > 0) then'//new_line('a')// &
            '            return'//new_line('a')// &
            '        end if'//new_line('a')// &
            '    class default'//new_line('a')// &
            '        return'//new_line('a')// &
            '    end select'//new_line('a')// &
            '    deallocate (obj)'//new_line('a')// &
            '    current_type = "child"'//new_line('a')// &
            '    flush (1)'//new_line('a')// &
            '    i = 1'//new_line('a')// &
            '    do while (i <= 2)'//new_line('a')// &
            '        if (value < 0) then'//new_line('a')// &
            '            return'//new_line('a')// &
            '        end if'//new_line('a')// &
            '        body_names: block'//new_line('a')// &
            '            character(len=:), allocatable :: next_type'//new_line('a')// &
            '            call find_parent(current_type, next_type)'//new_line('a')// &
            '            call move_alloc(next_type, current_type)'//new_line('a')// &
            '            if (value > 0) then'//new_line('a')// &
            '                current_type = "parent"'//new_line('a')// &
            '            end if'//new_line('a')// &
            '        end block body_names'//new_line('a')// &
            '        i = i + 1'//new_line('a')// &
            '    end do'//new_line('a')// &
            'end subroutine check'//new_line('a')// &
            'end module nested_procedure_constructs'//new_line('a')
    end function source_text

end program test_nested_procedure_constructs
