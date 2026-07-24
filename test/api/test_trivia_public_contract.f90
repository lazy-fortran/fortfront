program test_trivia_public_contract
    ! Pins the lexical-trivia surface that fluff consumes through the fortfront
    ! facade. fluff's src/fluff_ast/fluff_ast.f90 imports get_trivia_for_ast_node
    ! and trivia_t, and its rules compare kinds against CST_COMMENT,
    ! CST_WHITESPACE, and CST_NEWLINE. Removing or reshaping any of these is a
    ! breaking change for a downstream repository, so this test imports exactly
    ! what fluff imports and exercises it end to end.
    use fortfront, only: tooling_load_ast_from_string, ast_arena_t, &
        create_ast_arena, get_node_type_id_from_arena, &
        get_trivia_for_ast_node, &
        trivia_t, CST_COMMENT, CST_WHITESPACE, CST_NEWLINE
    use fortfront_types, only: NODE_ASSIGNMENT
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call require_kind_constants_distinct()
    call require_comment_trivia_reaches_consumer()

    write (*, '(a,i0,a,i0,a)') 'trivia_public_contract: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine require_kind_constants_distinct()
        ! fluff branches on these values, so they must stay distinct and stable.
        call assert(CST_COMMENT /= CST_WHITESPACE .and. &
            CST_WHITESPACE /= CST_NEWLINE .and. &
            CST_COMMENT /= CST_NEWLINE, &
            'trivia kind constants are distinct')
    end subroutine require_kind_constants_distinct

    subroutine require_comment_trivia_reaches_consumer()
        type(ast_arena_t) :: arena
        type(trivia_t), allocatable :: leading(:), trailing(:)
        character(len=:), allocatable :: source
        character(len=:), allocatable :: message
        integer :: root_index, assignment_index, i
        logical :: found, saw_comment

        source = 'program p'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '    ! leading comment'//new_line('a')// &
            '    x = 1'//new_line('a')// &
            'end program p'//new_line('a')

        arena = create_ast_arena()
        call tooling_load_ast_from_string(source, arena, root_index, message)
        call assert(root_index > 0, 'fixture parses')
        if (root_index <= 0) return

        assignment_index = first_node_of_type(arena, NODE_ASSIGNMENT)
        call assert(assignment_index > 0, 'fixture contains an assignment node')
        if (assignment_index <= 0) return

        call get_trivia_for_ast_node(source, arena, assignment_index, leading, &
            trailing, found)
        call assert(found, 'trivia query resolves the assignment node')
        if (.not. found) return

        saw_comment = .false.
        if (allocated(leading)) then
            do i = 1, size(leading)
                if (leading(i)%kind == CST_COMMENT) then
                    saw_comment = .true.
                    call assert(allocated(leading(i)%text), &
                        'comment trivia carries its text')
                    if (allocated(leading(i)%text)) then
                        call assert(index(leading(i)%text, 'leading comment') > 0, &
                            'comment trivia text is the source comment')
                    end if
                end if
            end do
        end if
        call assert(saw_comment, &
            'a comment above a statement is reported as leading trivia')
    end subroutine require_comment_trivia_reaches_consumer

    integer function first_node_of_type(arena, node_type) result(node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_type
        integer :: i

        node_index = 0
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == node_type) then
                node_index = i
                return
            end if
        end do
    end function first_node_of_type

end program test_trivia_public_contract
