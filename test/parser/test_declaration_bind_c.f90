program test_declaration_bind_c
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, token_t
    use ast_nodes_data, only: declaration_node
    use ast_nodes_core, only: assignment_node, identifier_node
    implicit none

    logical :: ok

    ok = .true.

    call check('real(c_double), bind(c) :: v = 42.0d0', 'real(c_double)', &
        .false., '', ok)
    call check('integer(c_int), bind(c) :: v = 7', 'integer(c_int)', &
        .false., '', ok)
    call check('real(8), bind(c), save :: v = 1.0d0', 'real(8)', &
        .true., '', ok)
    call check('real(c_double), bind(c, name="cv") :: v = 3.0d0', &
        'real(c_double)', .false., 'cv', ok)
    call check_standalone_save(ok)

    if (.not. ok) then
        print *, 'FAIL: bind(c) variable declaration parsing'
        stop 1
    end if
    print *, 'PASS: bind(c) variable declaration parsing'

contains

    subroutine check(decl, expect_type, expect_save, expect_bind_name, ok)
        character(len=*), intent(in) :: decl, expect_type, expect_bind_name
        logical, intent(in) :: expect_save
        logical, intent(inout) :: ok

        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg, src
        integer :: root_index, i, n_assign, n_decl, n_stray_c

        src = 'module m'//new_line('A')//'implicit none'//new_line('A')// &
              decl//new_line('A')//'end module m'
        options = tooling_parse_options_t()
        options%run_semantics = .false.
        call tooling_load_ast_from_string(src, arena, root_index, error_msg, &
            options, tokens)

        write (*, '(A)') '==== '//decl

        n_assign = 0
        n_decl = 0
        n_stray_c = 0
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                n_decl = n_decl + 1
                write (*, '(A,L1,A,I0,A,L1)') '  decl type='//trim(node%type_name)// &
                    ' has_initializer=', node%has_initializer, &
                    ' initializer_index=', node%initializer_index, &
                    ' is_bind_c=', node%is_bind_c
                if (allocated(node%bind_name)) then
                    write (*, '(A)') '  bind_name='//node%bind_name
                end if
                call assert(trim(node%type_name) == expect_type, &
                    'type_name '//trim(node%type_name), ok)
                call assert(node%has_initializer, 'has_initializer', ok)
                call assert(node%initializer_index > 0, 'initializer_index', ok)
                call assert(node%is_bind_c, 'is_bind_c', ok)
                if (expect_save) then
                    call assert(node%is_save, 'is_save', ok)
                end if
                if (len_trim(expect_bind_name) > 0) then
                    call assert(allocated(node%bind_name), 'bind_name allocated', ok)
                    if (allocated(node%bind_name)) then
                        call assert(index(node%bind_name, expect_bind_name) > 0, &
                            'bind_name value', ok)
                    end if
                end if
            type is (assignment_node)
                n_assign = n_assign + 1
            type is (identifier_node)
                if (trim(node%name) == 'c') n_stray_c = n_stray_c + 1
            end select
        end do

        call assert(n_decl == 1, 'exactly one declaration', ok)
        call assert(n_assign == 0, 'no stray assignment node', ok)
        call assert(n_stray_c == 0, 'no stray identifier c', ok)
    end subroutine check

    subroutine check_standalone_save(ok)
        logical, intent(inout) :: ok
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        character(len=*), parameter :: src = &
            'program p'//new_line('A')//'integer :: k'//new_line('A')// &
            'save :: k'//new_line('A')//'end program p'
        integer :: root_index, i

        options = tooling_parse_options_t()
        options%run_semantics = .false.
        call tooling_load_ast_from_string(src, arena, root_index, error_msg, &
            options)
        call assert(len_trim(error_msg) == 0, 'standalone SAVE parses', ok)
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == 'k') then
                        call assert(node%is_save, 'standalone SAVE sets is_save', ok)
                        return
                    end if
                end if
            end select
        end do
        call assert(.false., 'standalone SAVE declaration found', ok)
    end subroutine check_standalone_save

    subroutine assert(cond, label, ok)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: label
        logical, intent(inout) :: ok

        if (.not. cond) then
            write (*, '(A)') '  ASSERT FAILED: '//label
            ok = .false.
        end if
    end subroutine assert

end program test_declaration_bind_c
