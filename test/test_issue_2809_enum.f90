program test_issue_2809_enum
    ! ENUM constructs must parse into a structured enum_node carrying the
    ! enumerator names, resolved values, and bind(c), not a bare error_node.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_legacy, only: enum_node
    use ast_error_nodes, only: error_node_t
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, enum_count, error_count

    call read_example('examples/f90/issue_2809_enum.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    enum_count = 0
    error_count = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (enum_node)
            enum_count = enum_count + 1
            call check_enum(n)
            type is (error_node_t)
            error_count = error_count + 1
        end select
    end do

    if (enum_count /= 1) then
        print *, 'FAIL: expected 1 enum_node, found ', enum_count
        error stop 1
    end if
    if (error_count > 0) then
        print *, 'FAIL: enum produced error_node(s): ', error_count
        error stop 1
    end if

    print *, 'PASS: ENUM parsed into structured enum_node'

contains

    subroutine check_enum(n)
        type(enum_node), intent(in) :: n

        if (.not. n%is_bind_c) then
            print *, 'FAIL: bind(c) attribute not captured'
            error stop 1
        end if
        if (.not. allocated(n%enumerator_names)) then
            print *, 'FAIL: enumerator names not captured'
            error stop 1
        end if
        if (size(n%enumerator_names) /= 3) then
            print *, 'FAIL: expected 3 enumerators, found ', &
                size(n%enumerator_names)
            error stop 1
        end if
        ! red defaults to 0, green explicit 5, blue auto-increments to 6
        if (trim(n%enumerator_names(1)%s) /= 'red' .or. &
            n%enumerator_values(1) /= 0) then
            print *, 'FAIL: red should be 0, got ', n%enumerator_values(1)
            error stop 1
        end if
        if (trim(n%enumerator_names(2)%s) /= 'green' .or. &
            n%enumerator_values(2) /= 5) then
            print *, 'FAIL: green should be 5, got ', n%enumerator_values(2)
            error stop 1
        end if
        if (trim(n%enumerator_names(3)%s) /= 'blue' .or. &
            n%enumerator_values(3) /= 6) then
            print *, 'FAIL: blue should auto-increment to 6, got ', &
                n%enumerator_values(3)
            error stop 1
        end if
    end subroutine check_enum

    subroutine fail_on_error(error_msg, phase)
        character(len=:), allocatable, intent(in) :: error_msg
        character(len=*), intent(in) :: phase
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: ', phase, ' error: ', trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine fail_on_error

    include 'common/read_example.inc'
end program test_issue_2809_enum
