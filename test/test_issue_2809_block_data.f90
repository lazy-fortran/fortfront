program test_issue_2809_block_data
    ! BLOCK DATA bodies must parse into structured declarations / COMMON / DATA
    ! nodes, not raw text literals. Walk the block_data_node body and assert the
    ! expected node kinds, with no literal_node text passthrough.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: block_data_node, declaration_node
    use ast_nodes_legacy, only: common_block_node
    use ast_nodes_misc, only: data_statement_node
    use ast_nodes_core, only: literal_node
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, bd_count

    call read_example('examples/f90/issue_2809_block_data.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    bd_count = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (block_data_node)
            bd_count = bd_count + 1
            call check_body(arena, n)
        end select
    end do

    if (bd_count /= 1) then
        print *, 'FAIL: expected 1 block_data_node, found ', bd_count
        error stop 1
    end if

    print *, 'PASS: BLOCK DATA body parsed into structured nodes'

contains

    subroutine check_body(arena, n)
        type(ast_arena_t), intent(in) :: arena
        type(block_data_node), intent(in) :: n
        integer :: j, idx
        logical :: saw_decl, saw_common, saw_data, saw_literal

        saw_decl = .false.
        saw_common = .false.
        saw_data = .false.
        saw_literal = .false.

        if (.not. allocated(n%statement_indices)) then
            print *, 'FAIL: block_data body has no statements'
            error stop 1
        end if

        do j = 1, size(n%statement_indices)
            idx = n%statement_indices(j)
            if (idx < 1 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (s => arena%entries(idx)%node)
                type is (declaration_node)
                saw_decl = .true.
                type is (common_block_node)
                saw_common = .true.
                type is (data_statement_node)
                saw_data = .true.
                type is (literal_node)
                saw_literal = .true.
            end select
        end do

        if (.not. saw_decl) then
            print *, 'FAIL: integer declaration not parsed in block data'
            error stop 1
        end if
        if (.not. saw_common) then
            print *, 'FAIL: COMMON not parsed in block data'
            error stop 1
        end if
        if (.not. saw_data) then
            print *, 'FAIL: DATA not parsed in block data'
            error stop 1
        end if
        if (saw_literal) then
            print *, 'FAIL: block data body still contains raw text literals'
            error stop 1
        end if
    end subroutine check_body

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
end program test_issue_2809_block_data
