program test_issue_2855_external_statement
    ! EXTERNAL and INTRINSIC specification statements in a program body must both
    ! survive parsing so a backend can diagnose the F2018 8.5.9 conflict (an entity
    ! cannot be both). Previously the external declaration was dropped because
    ! "external" was not lexed as a keyword.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: intrinsic_statement_node
    implicit none

    character(:), allocatable :: src, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, intrinsic_count, external_count

    call read_example('examples/f90/issue_2855_external_intrinsic_conflict.f90', src)

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    call fail_on_error(error_msg, 'lex')
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call fail_on_error(error_msg, 'parse')

    intrinsic_count = 0
    external_count = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (intrinsic_statement_node)
            intrinsic_count = intrinsic_count + 1
            type is (declaration_node)
            if (n%is_external) then
                if (allocated(n%var_name)) then
                    if (trim(n%var_name) == 'nint') external_count = external_count + 1
                end if
            end if
        end select
    end do

    if (intrinsic_count /= 1) then
        print *, 'FAIL: expected 1 intrinsic_statement_node, found ', intrinsic_count
        error stop 1
    end if
    if (external_count /= 1) then
        print *, 'FAIL: expected 1 external declaration for nint, found ', &
            external_count
        error stop 1
    end if

    print *, 'PASS: EXTERNAL statement survives alongside INTRINSIC in program body'

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
end program test_issue_2855_external_statement
