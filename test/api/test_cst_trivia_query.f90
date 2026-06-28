program test_cst_trivia_query
    use fortfront, only: tooling_load_ast_from_string, ast_arena_t, &
        get_trivia_for_ast_node, get_source_trivia_at, &
        get_node_type_id_from_arena, &
        get_trivia_for_ast_node_tokens, tokenize_core_with_trivia, &
        token_t
    use fortfront_types, only: NODE_ASSIGNMENT
    use cst_nodes, only: CST_COMMENT, CST_NEWLINE, CST_WHITESPACE, trivia_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CST Trivia Query API Tests ==='
    print *

    if (.not. test_source_trivia_at()) all_passed = .false.
    if (.not. test_trivia_for_assignment_node()) all_passed = .false.
    if (.not. test_trivia_for_assignment_node_reuse_tokens()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All CST trivia query tests passed!'
        stop 0
    else
        print *, 'Some CST trivia query tests failed!'
        stop 1
    end if

contains

    logical function test_source_trivia_at()
        character(len=*), parameter :: source = "! header" // new_line('A') // &
            "   x = 1"
        type(trivia_t), allocatable :: trivia(:)

        test_source_trivia_at = .true.
        print *, 'Testing get_source_trivia_at...'

        trivia = get_source_trivia_at(source, 1, 1)
        if (size(trivia) /= 1) then
            print *, '  FAIL: expected 1 trivia token at 1:1'
            test_source_trivia_at = .false.
            return
        end if
        if (trivia(1)%kind /= CST_COMMENT) then
            print *, '  FAIL: expected CST_COMMENT at 1:1'
            test_source_trivia_at = .false.
            return
        end if
        if (trim(trivia(1)%text) /= '! header') then
            print *, '  FAIL: unexpected comment text at 1:1'
            test_source_trivia_at = .false.
            return
        end if

        trivia = get_source_trivia_at(source, 1, 9)
        if (size(trivia) /= 1) then
            print *, '  FAIL: expected 1 trivia token at 1:9'
            test_source_trivia_at = .false.
            return
        end if
        if (trivia(1)%kind /= CST_NEWLINE) then
            print *, '  FAIL: expected CST_NEWLINE at 1:9'
            test_source_trivia_at = .false.
            return
        end if

        trivia = get_source_trivia_at(source, 2, 1)
        if (size(trivia) /= 1) then
            print *, '  FAIL: expected 1 trivia token at 2:1'
            test_source_trivia_at = .false.
            return
        end if
        if (trivia(1)%kind /= CST_WHITESPACE) then
            print *, '  FAIL: expected CST_WHITESPACE at 2:1'
            test_source_trivia_at = .false.
            return
        end if
        if (trivia(1)%text /= '   ') then
            print *, '  FAIL: unexpected whitespace text at 2:1'
            test_source_trivia_at = .false.
            return
        end if

        print *, '  PASS: get_source_trivia_at'
    end function test_source_trivia_at

    logical function test_trivia_for_assignment_node()
        character(len=*), parameter :: source = "! header" // new_line('A') // &
            "   x = 1"
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: i
        integer :: assignment_index
        type(trivia_t), allocatable :: leading(:), trailing(:)
        logical :: found

        test_trivia_for_assignment_node = .true.
        print *, 'Testing get_trivia_for_ast_node...'

        call tooling_load_ast_from_string(source, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: tooling_load_ast_from_string: ', trim(error_msg)
            test_trivia_for_assignment_node = .false.
            return
        end if

        assignment_index = 0
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == NODE_ASSIGNMENT) then
                assignment_index = i
                exit
            end if
        end do

        if (assignment_index == 0) then
            print *, '  FAIL: did not find assignment node'
            test_trivia_for_assignment_node = .false.
            return
        end if

        call get_trivia_for_ast_node(source, arena, assignment_index, leading, &
            trailing, found)
        if (.not. found) then
            print *, '  FAIL: trivia lookup returned found=false'
            test_trivia_for_assignment_node = .false.
            return
        end if

        if (size(leading) /= 3) then
            print *, '  FAIL: expected 3 leading trivia tokens'
            test_trivia_for_assignment_node = .false.
            return
        end if

        if (leading(1)%kind /= CST_COMMENT) then
            print *, '  FAIL: expected CST_COMMENT as first leading trivia'
            test_trivia_for_assignment_node = .false.
            return
        end if
        if (trim(leading(1)%text) /= '! header') then
            print *, '  FAIL: unexpected leading comment text'
            test_trivia_for_assignment_node = .false.
            return
        end if

        if (leading(2)%kind /= CST_NEWLINE) then
            print *, '  FAIL: expected CST_NEWLINE as second leading trivia'
            test_trivia_for_assignment_node = .false.
            return
        end if

        if (leading(3)%kind /= CST_WHITESPACE) then
            print *, '  FAIL: expected CST_WHITESPACE as third leading trivia'
            test_trivia_for_assignment_node = .false.
            return
        end if
        if (leading(3)%text /= '   ') then
            print *, '  FAIL: unexpected leading whitespace text'
            test_trivia_for_assignment_node = .false.
            return
        end if

        if (size(trailing) /= 1) then
            print *, '  FAIL: expected 1 trailing trivia token for node start'
            test_trivia_for_assignment_node = .false.
            return
        end if
        if (trailing(1)%kind /= CST_WHITESPACE) then
            print *, '  FAIL: expected CST_WHITESPACE as trailing trivia'
            test_trivia_for_assignment_node = .false.
            return
        end if
        if (trailing(1)%text /= ' ') then
            print *, '  FAIL: unexpected trailing whitespace text'
            test_trivia_for_assignment_node = .false.
            return
        end if

        print *, '  PASS: get_trivia_for_ast_node'
    end function test_trivia_for_assignment_node

    logical function test_trivia_for_assignment_node_reuse_tokens()
        character(len=*), parameter :: source = "! header" // new_line('A') // &
            "   x = 1"
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: i
        integer :: assignment_index
        type(token_t), allocatable :: tokens(:)
        type(trivia_t), allocatable :: leading_1(:), trailing_1(:)
        type(trivia_t), allocatable :: leading_2(:), trailing_2(:)
        type(trivia_t), allocatable :: leading_ref(:), trailing_ref(:)
        logical :: found_1, found_2, found_ref

        test_trivia_for_assignment_node_reuse_tokens = .true.
        print *, 'Testing get_trivia_for_ast_node_tokens reuse...'

        call tooling_load_ast_from_string(source, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: tooling_load_ast_from_string: ', trim(error_msg)
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        assignment_index = 0
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == NODE_ASSIGNMENT) then
                assignment_index = i
                exit
            end if
        end do

        if (assignment_index == 0) then
            print *, '  FAIL: did not find assignment node'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        call tokenize_core_with_trivia(source, tokens)

        call get_trivia_for_ast_node_tokens(tokens, arena, assignment_index, &
            leading_1, trailing_1, found_1)
        call get_trivia_for_ast_node_tokens(tokens, arena, assignment_index, &
            leading_2, trailing_2, found_2)
        call get_trivia_for_ast_node(source, arena, assignment_index, leading_ref, &
            trailing_ref, found_ref)

        if (.not. found_1 .or. .not. found_2 .or. .not. found_ref) then
            print *, '  FAIL: expected found=true for all trivia queries'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        if (.not. trivia_equal(leading_1, leading_2)) then
            print *, '  FAIL: token-reused leading trivia mismatch between calls'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        if (.not. trivia_equal(trailing_1, trailing_2)) then
            print *, '  FAIL: token-reused trailing trivia mismatch between calls'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        if (.not. trivia_equal(leading_1, leading_ref)) then
            print *, '  FAIL: leading trivia differs from source-based API'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        if (.not. trivia_equal(trailing_1, trailing_ref)) then
            print *, '  FAIL: trailing trivia differs from source-based API'
            test_trivia_for_assignment_node_reuse_tokens = .false.
            return
        end if

        print *, '  PASS: get_trivia_for_ast_node_tokens reuse'
    end function test_trivia_for_assignment_node_reuse_tokens

    logical function trivia_equal(a, b)
        type(trivia_t), intent(in) :: a(:)
        type(trivia_t), intent(in) :: b(:)

        integer :: i

        trivia_equal = .false.
        if (size(a) /= size(b)) return

        do i = 1, size(a)
            if (a(i)%kind /= b(i)%kind) return
            if (a(i)%text /= b(i)%text) return
        end do

        trivia_equal = .true.
    end function trivia_equal

end program test_cst_trivia_query
