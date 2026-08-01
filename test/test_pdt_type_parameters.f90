program test_pdt_type_parameters
    ! Issue #2952: derived-type parameters (PDT) must be represented in the AST
    ! and reachable through public compiler-facing queries:
    !   get_derived_type_parameters (formals: name, KIND/LEN, default)
    !   get_declaration_type_parameters (actuals on an entity declaration)
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, &
        compiler_frontend_result_t, INPUT_MODE_STANDARD, &
        get_node_type_at
    use fortfront_compiler, only: get_derived_type_parameters, &
        get_declaration_type_parameters, &
        type_parameter_t, &
        PARAM_KIND, PARAM_LEN
    implicit none

    call test_type_parameter_formals()
    call test_type_parameter_actuals()
    call test_empty_type_parameter_list_rejected()

    print *, 'PASS: derived-type parameters are represented and queryable'

contains

    function pdt_source() result(src)
        character(:), allocatable :: src

        src = 'program t'//new_line('a')// &
            '  type :: box_t(n, k)'//new_line('a')// &
            '    integer, len :: n'//new_line('a')// &
            '    integer, kind :: k = 4'//new_line('a')// &
            '    integer :: tag'//new_line('a')// &
            '  end type box_t'//new_line('a')// &
            '  type(box_t(3, 8)) :: a'//new_line('a')// &
            'end program t'
    end function pdt_source

    subroutine compile_pdt(result)
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(pdt_source(), result, options)
        if (.not. result%success()) then
            print *, 'FAIL: frontend rejected PDT source: ', &
                trim(result%diagnostic_text)
            error stop 1
        end if
    end subroutine compile_pdt

    subroutine test_type_parameter_formals()
        type(compiler_frontend_result_t) :: result
        type(type_parameter_t), allocatable :: params(:)
        integer :: i
        logical :: ok

        call compile_pdt(result)

        ok = .false.
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
            call get_derived_type_parameters(result%arena, i, params)
            if (size(params) /= 2) cycle
            if (params(1)%name /= 'n') cycle
            if (params(1)%classification /= PARAM_LEN) cycle
            if (params(2)%name /= 'k') cycle
            if (params(2)%classification /= PARAM_KIND) cycle
            if (params(2)%default_index <= 0) cycle
            ok = .true.
        end do

        if (.not. ok) then
            print *, 'FAIL: type-parameter formals not represented on derived type'
            error stop 1
        end if
    end subroutine test_type_parameter_formals

    subroutine test_type_parameter_actuals()
        type(compiler_frontend_result_t) :: result
        integer, allocatable :: actuals(:)
        integer :: i
        logical :: ok

        call compile_pdt(result)

        ok = .false.
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= 'declaration') cycle
            call get_declaration_type_parameters(result%arena, i, actuals)
            if (size(actuals) == 2) ok = .true.
        end do

        if (.not. ok) then
            print *, 'FAIL: type-parameter actuals not captured on declaration'
            error stop 1
        end if
    end subroutine test_type_parameter_actuals

    subroutine test_empty_type_parameter_list_rejected()
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        character(:), allocatable :: source

        source = 'program t'//new_line('a')// &
            '  type :: box_t()'//new_line('a')// &
            '  end type box_t'//new_line('a')// &
            'end program t'
        options = compiler_frontend_options_t()
        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, options)
        if (result%success()) then
            print *, 'FAIL: empty PDT parameter list was accepted'
            error stop 1
        end if
        if (index(result%diagnostic_text, 'type parameter list') == 0) then
            print *, 'FAIL: empty PDT parameter list diagnostic missing'
            print *, trim(result%diagnostic_text)
            error stop 1
        end if
    end subroutine test_empty_type_parameter_list_rejected

end program test_pdt_type_parameters
