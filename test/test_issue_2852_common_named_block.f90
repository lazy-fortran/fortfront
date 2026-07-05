program test_issue_2852_common_named_block
    ! A COMMON block whose name is "block" must keep its name: "block" sits in a
    ! name position, not the BLOCK construct keyword, so parsing must preserve it.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_legacy, only: common_block_node
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, common_count, named_count

    call read_example('examples/f90/issue_2852_common_block_named_block.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    common_count = 0
    named_count = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (common_block_node)
            common_count = common_count + 1
            if (block_is(n, 'block') .and. member_is(n, 1, 'x')) &
                named_count = named_count + 1
        end select
    end do

    if (common_count /= 2) then
        print *, 'FAIL: expected 2 common_block_node, found ', common_count
        error stop 1
    end if
    if (named_count /= 2) then
        print *, 'FAIL: block name "block" with member x not preserved (', &
            named_count, ' of 2)'
        error stop 1
    end if

    print *, 'PASS: COMMON block named "block" keeps its name'

contains

    logical function block_is(n, expected) result(ok)
        type(common_block_node), intent(in) :: n
        character(len=*), intent(in) :: expected
        ok = .false.
        if (.not. allocated(n%block_names)) return
        if (size(n%block_names) < 1) return
        if (allocated(n%block_names(1)%s)) ok = trim(n%block_names(1)%s) == expected
    end function block_is

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
end program test_issue_2852_common_named_block
