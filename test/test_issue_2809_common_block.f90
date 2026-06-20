program test_issue_2809_common_block
    ! COMMON statements must parse into structured common_block_node AST nodes
    ! (block name + ordered members), not survive as comment_node text.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_legacy, only: common_block_node
    use ast_nodes_misc, only: comment_node
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, common_count, comment_count
    logical :: saw_shared, saw_blank

    call read_example('examples/f90/issue_2809_common_block.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    common_count = 0
    comment_count = 0
    saw_shared = .false.
    saw_blank = .false.
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
        type is (common_block_node)
            common_count = common_count + 1
            call inspect_block(n, saw_shared, saw_blank)
        type is (comment_node)
            comment_count = comment_count + 1
        end select
    end do

    if (common_count /= 2) then
        print *, 'FAIL: expected 2 common_block_node, found ', common_count
        error stop 1
    end if
    if (.not. saw_shared) then
        print *, 'FAIL: named block /shared/ with members a, b not found'
        error stop 1
    end if
    if (.not. saw_blank) then
        print *, 'FAIL: blank common with members c, d not found'
        error stop 1
    end if
    if (comment_count > 0) then
        print *, 'FAIL: COMMON survived as comment_node (', comment_count, ')'
        error stop 1
    end if

    print *, 'PASS: COMMON parsed into structured common_block_node'

contains

    subroutine inspect_block(n, saw_shared, saw_blank)
        type(common_block_node), intent(in) :: n
        logical, intent(inout) :: saw_shared, saw_blank
        character(len=:), allocatable :: name

        if (.not. allocated(n%block_names)) return
        if (size(n%block_names) < 1) return
        name = ''
        if (allocated(n%block_names(1)%s)) name = trim(n%block_names(1)%s)
        if (name == 'shared') then
            if (member_count(n) == 2 .and. member_is(n, 1, 'a') .and. &
                member_is(n, 2, 'b')) saw_shared = .true.
        else if (len_trim(name) == 0) then
            if (member_count(n) == 2 .and. member_is(n, 1, 'c') .and. &
                member_is(n, 2, 'd')) saw_blank = .true.
        end if
    end subroutine inspect_block

    integer function member_count(n) result(c)
        type(common_block_node), intent(in) :: n
        c = 0
        if (allocated(n%member_names)) c = size(n%member_names)
    end function member_count

    logical function member_is(n, idx, expected) result(ok)
        type(common_block_node), intent(in) :: n
        integer, intent(in) :: idx
        character(len=*), intent(in) :: expected
        ok = .false.
        if (.not. allocated(n%member_names)) return
        if (idx > size(n%member_names)) return
        if (allocated(n%member_names(idx)%s)) ok = trim(n%member_names(idx)%s) == &
                                                   expected
    end function member_is

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
end program test_issue_2809_common_block
