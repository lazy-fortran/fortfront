program test_compiler_node_queries
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: ast_arena_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, INPUT_MODE_STANDARD, &
        is_declaration_node, is_derived_type_node, &
        get_declaration_var_name, get_declaration_type_name, &
        get_declaration_has_initializer, get_declaration_initializer_index, &
        get_declaration_initializer, &
        get_derived_type_name, &
        get_node_stmt_label, get_goto_label, goto_is_computed, &
        get_goto_label_list, get_goto_selector_index
    implicit none

    call test_declaration_queries()
    call test_derived_type_name_query()
    call test_label_and_goto_queries()
    print *, 'PASS: compiler node queries'

contains

    subroutine test_declaration_queries()
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: name, type_name, error_msg
        integer :: decl_index
        integer :: init_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer, parameter :: n = 7'//new_line('a')// &
            '  real :: x'//new_line('a')// &
            'end program p', result)

        decl_index = find_declaration(result%arena, 'n')
        call get_declaration_var_name(result%arena, decl_index, name, error_msg)
        call require_no_error(error_msg)
        call require_equal(name, 'n', 'declaration name')
        call get_declaration_type_name(result%arena, decl_index, type_name, &
            error_msg)
        call require_no_error(error_msg)
        call require_equal(type_name, 'integer', 'declaration type')
        if (.not. get_declaration_has_initializer(result%arena, decl_index)) then
            call fail('parameter declaration initializer not reported')
        end if
        init_index = get_declaration_initializer_index(result%arena, decl_index)
        if (init_index <= 0) call fail('initializer index not returned')
        if (init_index /= get_declaration_initializer(result%arena, decl_index)) then
            call fail('initializer query mismatch')
        end if
    end subroutine test_declaration_queries

    subroutine test_derived_type_name_query()
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: name, error_msg
        integer :: type_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  type :: point_t'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '  end type point_t'//new_line('a')// &
            'end program p', result)

        type_index = find_derived_type(result%arena)
        call get_derived_type_name(result%arena, type_index, name, error_msg)
        call require_no_error(error_msg)
        call require_equal(name, 'point_t', 'derived type name')
    end subroutine test_derived_type_name_query

    subroutine test_label_and_goto_queries()
        type(compiler_frontend_result_t) :: result
        integer :: i
        integer :: simple_goto
        integer :: computed_goto
        logical :: saw_label
        character(len=:), allocatable :: label
        character(len=:), allocatable :: label_list

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: k'//new_line('a')// &
            '  k = 1'//new_line('a')// &
            '  goto 100'//new_line('a')// &
            '  goto (100, 200), k'//new_line('a')// &
            '100 continue'//new_line('a')// &
            '200 continue'//new_line('a')// &
            'end program p', result)

        simple_goto = 0
        computed_goto = 0
        saw_label = .false.
        do i = 1, result%arena%size
            label = get_node_stmt_label(result%arena, i)
            if (trim(label) == '100') saw_label = .true.
            if (goto_is_computed(result%arena, i)) then
                computed_goto = i
            else
                label = get_goto_label(result%arena, i)
                if (trim(label) == '100') simple_goto = i
            end if
        end do

        if (.not. saw_label) call fail('statement label not exposed')
        if (simple_goto <= 0) call fail('simple goto label not exposed')
        if (computed_goto <= 0) call fail('computed goto not exposed')
        label_list = get_goto_label_list(result%arena, computed_goto)
        if (index(label_list, '100') <= 0 .or. index(label_list, '200') <= 0) then
            call fail('computed goto labels not exposed')
        end if
        if (get_goto_selector_index(result%arena, computed_goto) <= 0) then
            call fail('computed goto selector not exposed')
        end if
    end subroutine test_label_and_goto_queries

    subroutine compile_standard(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)
        if (result%success()) return
        write (error_unit, '(A)') 'FAIL: frontend rejected test source'
        if (allocated(result%error_msg)) write (error_unit, '(A)') result%error_msg
        error stop 1
    end subroutine compile_standard

    integer function find_declaration(arena, expected_name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: expected_name
        character(len=:), allocatable :: name, error_msg
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. is_declaration_node(arena, i)) cycle
            call get_declaration_var_name(arena, i, name, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(name) == trim(expected_name)) then
                index = i
                return
            end if
        end do
        call fail('declaration not found: '//trim(expected_name))
    end function find_declaration

    integer function find_derived_type(arena) result(index)
        type(ast_arena_t), intent(in) :: arena
        integer :: i

        index = 0
        do i = 1, arena%size
            if (is_derived_type_node(arena, i)) then
                index = i
                return
            end if
        end do
        call fail('derived type not found')
    end function find_derived_type

    subroutine require_no_error(error_msg)
        character(len=*), intent(in) :: error_msg

        if (len_trim(error_msg) == 0) return
        call fail(error_msg)
    end subroutine require_no_error

    subroutine require_equal(actual, expected, label)
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: label

        if (trim(actual) == trim(expected)) return
        call fail(trim(label)//' mismatch')
    end subroutine require_equal

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_compiler_node_queries
