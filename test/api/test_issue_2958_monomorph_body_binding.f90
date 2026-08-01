program test_issue_2958_monomorph_body_binding
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_nodes_core, only: identifier_node
    use ast_nodes_procedure, only: function_def_node
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_LAZY, declaration_binding_t, resolve_name_at_node
    implicit none

    call test_monomorphic_copies_own_their_body()
    print *, 'PASS: monomorphic copies own their body bindings'

contains

    subroutine test_monomorphic_copies_own_their_body()
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        integer :: i, j, func_index, checked
        integer, allocatable :: body(:)
        character(len=:), allocatable :: fname

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_LAZY
        options%run_semantics = .true.
        options%standardize = .true.
        call compile_frontend_from_string( &
            'function twice(x)'//new_line('a')// &
            '  twice = 2 * x'//new_line('a')// &
            'end function'//new_line('a')// &
            'print *, twice(3)'//new_line('a')// &
            'print *, twice(2.5)'//new_line('a'), result, options)
        if (.not. result%success()) then
            write (error_unit, '(A)') 'FAIL: frontend rejected reproducer'
            if (allocated(result%error_msg)) then
                write (error_unit, '(A)') result%error_msg
            end if
            error stop 1
        end if

        checked = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                fname = trim(node%name)
                if (index(fname, 'twice__') /= 1) cycle
                if (.not. allocated(node%body_indices)) cycle
                body = node%body_indices
                func_index = i
            class default
                cycle
            end select
            do j = 1, size(body)
                call check_subtree(result, body(j), func_index, fname, checked)
            end do
        end do

        if (checked == 0) then
            write (error_unit, '(A)') &
                'FAIL: no monomorphic copy body reference to x was found'
            error stop 1
        end if
    end subroutine test_monomorphic_copies_own_their_body

    recursive subroutine check_subtree(result, node_index, func_index, fname, &
                                       checked)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: node_index
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: fname
        integer, intent(inout) :: checked
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: k

        if (node_index < 1 .or. node_index > result%arena%size) return
        if (.not. result%arena%has_node_at(node_index)) return
        select type (node => result%arena%entries(node_index)%node)
        type is (identifier_node)
            if (.not. allocated(node%name)) return
            if (trim(node%name) /= 'x') return
            call resolve_name_at_node(result%arena, node_index, 'x', binding, &
                                      error_msg)
            checked = checked + 1
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'FAIL: '//trim(error_msg)
                error stop 1
            end if
            if (.not. binding%found) then
                write (error_unit, '(A)') 'FAIL: x unresolved in '//fname
                error stop 1
            end if
            if (binding%scope_node_index /= func_index) then
                write (error_unit, '(A)') &
                    'FAIL: x in '//fname//' binds outside its own procedure'
                error stop 1
            end if
        end select
        do k = 1, result%arena%size
            if (.not. result%arena%has_node_at(k)) cycle
            if (result%arena%entries(k)%parent_index /= node_index) cycle
            call check_subtree(result, k, func_index, fname, checked)
        end do
    end subroutine check_subtree

end program test_issue_2958_monomorph_body_binding
