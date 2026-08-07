program test_interface_procedure_reference
    ! Specific names in an explicit interface and ENTRY names are procedure
    ! names in the enclosing scope, not implicitly typed data references.
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: source

    options%run_semantics = .true.
    options%input_mode = INPUT_MODE_STANDARD
    options%standardize = .false.

    source = 'subroutine host()'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  interface'//new_line('a')// &
        '    subroutine external_proc()'//new_line('a')// &
        '    end subroutine external_proc'//new_line('a')// &
        '  end interface'//new_line('a')// &
        '  external_proc = 1'//new_line('a')// &
        'end subroutine host'
    call compile_frontend_from_string(source, result, options)
    call assert_not_implicit_name(result, 'external_proc')

    source = 'function host(x) result(r)'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  integer :: x, r'//new_line('a')// &
        '  r = x'//new_line('a')// &
        '  entry alternate(x) result(r2)'//new_line('a')// &
        '  alternate = x + 1'//new_line('a')// &
        'end function host'
    call compile_frontend_from_string(source, result, options)
    call assert_not_implicit_name(result, 'alternate')

    print *, 'PASS: interface and ENTRY procedure names bypass implicit-variable checks'

contains

    subroutine assert_not_implicit_name(frontend_result, name)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: marker

        marker = "Name '"//trim(name)//"' is not declared under IMPLICIT NONE"
        if (frontend_result%success()) return
        if (index(frontend_result%diagnostic_text, marker) /= 0) then
            write (*, '(A)') 'FAIL: procedure name was treated as an implicit variable: '// &
                trim(frontend_result%diagnostic_text)
            error stop 1
        end if
    end subroutine assert_not_implicit_name

end program test_interface_procedure_reference
