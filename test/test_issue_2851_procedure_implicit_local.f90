program test_issue_2851_procedure_implicit_local
    ! Issue #2851: the implicit-typing declaration-synthesis pass must also run
    ! for procedure bodies, not only the implicit-main program. An implicitly
    ! typed local inside a subroutine/function body must get a declaration_node
    ! in the arena so AST consumers do not see an undeclared identifier.
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        INPUT_MODE_LAZY, get_node_type_at, declaration_node
    implicit none

    call assert_local_declared('subroutine', &
        'subroutine s(n)'//new_line('a')// &
        '  integer :: n'//new_line('a')// &
        '  m = n + 1'//new_line('a')// &
        '  print *, m'//new_line('a')// &
        'end subroutine', 'm')

    call assert_local_declared('function', &
        'function f(n)'//new_line('a')// &
        '  integer :: n'//new_line('a')// &
        '  k = n + 2'//new_line('a')// &
        '  f = k'//new_line('a')// &
        'end function', 'k')

    print *, 'PASS: procedure implicit locals get declaration_nodes'

contains

    subroutine assert_local_declared(label, src, local_name)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: src
        character(len=*), intent(in) :: local_name
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        integer :: i
        logical :: found

        options = compiler_frontend_options_t()
        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_LAZY
        options%standardize = .true.
        call compile_frontend_from_string(src, result, options)
        if (.not. result%success()) then
            print *, 'FAIL: ', label, ' rejected: ', trim(result%diagnostic_text)
            error stop 1
        end if

        found = .false.
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= 'declaration') cycle
            select type (nd => result%arena%entries(i)%node)
                type is (declaration_node)
                if (trim(nd%var_name) /= local_name) cycle
                if (trim(nd%type_name) /= 'integer') then
                    print *, 'FAIL: ', label, ' local ', local_name, &
                        ' has type ', trim(nd%type_name), ' not integer'
                    error stop 1
                end if
                found = .true.
            end select
        end do

        if (.not. found) then
            print *, 'FAIL: ', label, ' local ', local_name, &
                ' has no declaration_node in the arena'
            error stop 1
        end if
    end subroutine assert_local_declared

end program test_issue_2851_procedure_implicit_local
