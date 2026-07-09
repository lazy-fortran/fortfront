program test_compiler_scope_resolution
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node, module_node
    use ast_nodes_procedure, only: subroutine_def_node
    use fortfront_compiler, only: compiler_frontend_options_t, &
                             compiler_frontend_result_t, compile_frontend_from_string, &
                                  INPUT_MODE_STANDARD, declaration_binding_t, &
                                  resolve_name_in_scope, resolve_name_at_node, &
                               BINDING_NAMED_CONSTANT, ASSOCIATION_HOST, ASSOCIATION_USE
    implicit none

    call test_host_parameter()
    call test_use_rename()
    call test_private_module_name_hidden()
    print *, 'PASS: compiler scope resolution queries'

contains

    subroutine test_host_parameter()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: subroutine_index
        integer :: decl_index
        integer :: array_decl_index
        integer :: bound_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  integer, parameter :: n = 4'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine s()'//new_line('a')// &
            '    integer :: a(n)'//new_line('a')// &
            '  end subroutine s'//new_line('a')// &
            'end program p', result)
        subroutine_index = find_subroutine(result, 's')
        decl_index = find_declaration(result, 'n')
        array_decl_index = find_declaration(result, 'a')
        bound_index = first_dimension_index(result, array_decl_index)
        call resolve_name_in_scope(result%arena, subroutine_index, 'n', binding, &
                                   error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
                             ASSOCIATION_HOST, 'host parameter n')
        call resolve_name_at_node(result%arena, array_decl_index, 'n', binding, &
                                  error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
                             ASSOCIATION_HOST, 'host parameter n at declaration')
        call resolve_name_at_node(result%arena, bound_index, 'n', binding, &
                                  error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
                             ASSOCIATION_HOST, 'host parameter n at bound')
    end subroutine test_host_parameter

    subroutine test_use_rename()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: program_index
        integer :: decl_index

        call compile_standard( &
            'module m'//new_line('a')// &
            '  integer, parameter :: remote = 7'//new_line('a')// &
            'end module m'//new_line('a')// &
            'program p'//new_line('a')// &
            '  use m, only: local => remote'//new_line('a')// &
            '  integer :: a(local)'//new_line('a')// &
            'end program p', result)
        program_index = find_program(result, 'p')
        decl_index = find_declaration(result, 'remote')
        call resolve_name_in_scope(result%arena, program_index, 'local', binding, &
                                   error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
                             ASSOCIATION_USE, 'use-renamed local')
        if (trim(binding%module_name) /= 'm') then
            write (error_unit, '(A)') 'FAIL: use binding module_name mismatch'
            error stop 1
        end if
        if (trim(binding%remote_name) /= 'remote') then
            write (error_unit, '(A)') 'FAIL: use binding remote_name mismatch'
            error stop 1
        end if
    end subroutine test_use_rename

    subroutine test_private_module_name_hidden()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: program_index

        call compile_standard( &
            'module m'//new_line('a')// &
            '  private :: hidden'//new_line('a')// &
            '  integer, parameter :: hidden = 3'//new_line('a')// &
            'end module m'//new_line('a')// &
            'program p'//new_line('a')// &
            '  use m'//new_line('a')// &
            'end program p', result)
        program_index = find_program(result, 'p')
        call resolve_name_in_scope(result%arena, program_index, 'hidden', binding, &
                                   error_msg)
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(error_msg)
            error stop 1
        end if
        if (binding%found) then
            write (error_unit, '(A)') 'FAIL: private module name resolved through use'
            error stop 1
        end if
    end subroutine test_private_module_name_hidden

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

    subroutine require_binding(error_msg, binding, node_index, kind, association, &
                               label)
        character(len=*), intent(in) :: error_msg
        type(declaration_binding_t), intent(in) :: binding
        integer, intent(in) :: node_index
        integer, intent(in) :: kind
        integer, intent(in) :: association
        character(len=*), intent(in) :: label

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(error_msg)
            error stop 1
        end if
        if (.not. binding%found) then
            write (error_unit, '(A)') 'FAIL: unresolved '//trim(label)
            error stop 1
        end if
        if (binding%node_index /= node_index) then
            write (error_unit, '(A)') 'FAIL: wrong node for '//trim(label)
            error stop 1
        end if
        if (binding%binding_kind /= kind) then
            write (error_unit, '(A)') 'FAIL: wrong binding kind for '//trim(label)
            error stop 1
        end if
        if (binding%association /= association) then
            write (error_unit, '(A)') 'FAIL: wrong association for '//trim(label)
            error stop 1
        end if
    end subroutine require_binding

    integer function find_program(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (program_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: program not found: '//trim(name)
        error stop 1
    end function find_program

    integer function find_subroutine(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (subroutine_def_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: subroutine not found: '//trim(name)
        error stop 1
    end function find_subroutine

    integer function find_declaration(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (declaration_node)
                if (declaration_names(node, name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: declaration not found: '//trim(name)
        error stop 1
    end function find_declaration

    logical function declaration_names(node, name) result(matches)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: name
        integer :: i

        matches = .false.
        if (allocated(node%var_name)) then
            if (trim(node%var_name) == trim(name)) then
                matches = .true.
                return
            end if
        end if
        if (.not. allocated(node%var_names)) return
        do i = 1, size(node%var_names)
            if (trim(node%var_names(i)) == trim(name)) then
                matches = .true.
                return
            end if
        end do
    end function declaration_names

    integer function first_dimension_index(result, decl_index) result(dim_index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: decl_index

        dim_index = 0
        if (.not. result%arena%has_node_at(decl_index)) return
        select type (node => result%arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. allocated(node%dimension_indices)) return
            if (size(node%dimension_indices) < 1) return
            dim_index = node%dimension_indices(1)
        end select
        if (dim_index > 0) return
        write (error_unit, '(A)') 'FAIL: declaration has no dimension'
        error stop 1
    end function first_dimension_index

end program test_compiler_scope_resolution
