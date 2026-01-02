program test_issue_2292_use_preservation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        & iostat_end, iostat_eor
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: subroutine_def_node
    use ast_nodes_misc, only: use_statement_node
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index

    call read_example('examples/f90/issue_2292_use_preservation.f90', source_code)

    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: lexer error: ' // trim(error_msg)
        error stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: parser error: ' // trim(error_msg)
        error stop 1
    end if

    if (root_index <= 0 .or. root_index > arena%size) then
        write (error_unit, '(A)') 'FAIL: parser did not return a valid root index'
        error stop 1
    end if

    call assert_use_survives(arena, root_index)

    print *, 'PASS: USE statements survive inside wrapped subroutines'

contains

    include '../common/read_example.inc'


    subroutine assert_use_survives(arena, root_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        logical :: found_subroutine
        integer :: i, idx

        if (.not. allocated(arena%entries(root_index)%node)) then
            write (error_unit, '(A)') 'FAIL: root node not allocated'
            error stop 1
        end if

        found_subroutine = .false.
        select type (prog => arena%entries(root_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) then
                write (error_unit, '(A)') 'FAIL: program body indices not allocated'
                error stop 1
            end if
            do i = 1, size(prog%body_indices)
                idx = prog%body_indices(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (sub => arena%entries(idx)%node)
                type is (subroutine_def_node)
                    found_subroutine = .true.
                    call assert_subroutine_has_use(arena, sub)
                    return
                end select
            end do
        class default
            write (error_unit, '(A)') 'FAIL: root node is not a program'
            error stop 1
        end select

        if (.not. found_subroutine) then
            write (error_unit, '(A)') 'FAIL: no subroutine definition found'
            error stop 1
        end if
    end subroutine assert_use_survives

    subroutine assert_subroutine_has_use(arena, sub_def)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub_def
        integer :: i, idx

        if (.not. allocated(sub_def%body_indices)) then
            write (error_unit, '(A)') 'FAIL: subroutine body not allocated'
            error stop 1
        end if

        do i = 1, size(sub_def%body_indices)
            idx = sub_def%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (use_statement_node)
                return
            end select
        end do

        write (error_unit, '(A)') 'FAIL: subroutine lost USE statement'
        error stop 1
    end subroutine assert_subroutine_has_use

end program test_issue_2292_use_preservation
