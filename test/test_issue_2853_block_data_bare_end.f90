program test_issue_2853_block_data_bare_end
    ! A BLOCK DATA unit terminated by a bare END (F2018 R1420) must not absorb
    ! the following program unit. Parsing must yield one block_data_node and a
    ! separate program_node.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: block_data_node
    use ast_nodes_core, only: program_node
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, bd_count, prog_count
    logical :: saw_program_p

    call read_example('examples/f90/issue_2853_block_data_bare_end.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    bd_count = 0
    prog_count = 0
    saw_program_p = .false.
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (block_data_node)
            bd_count = bd_count + 1
            type is (program_node)
            prog_count = prog_count + 1
            if (allocated(n%name)) then
                if (trim(n%name) == 'p') saw_program_p = .true.
            end if
        end select
    end do

    if (bd_count /= 1) then
        print *, 'FAIL: expected 1 block_data_node, found ', bd_count
        error stop 1
    end if
    if (prog_count /= 1) then
        print *, 'FAIL: expected 1 program_node, found ', prog_count
        error stop 1
    end if
    if (.not. saw_program_p) then
        print *, 'FAIL: program p not parsed as a separate unit'
        error stop 1
    end if

    print *, 'PASS: bare END terminates BLOCK DATA; program p is a separate unit'

contains

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
end program test_issue_2853_block_data_bare_end
