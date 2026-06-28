program test_implied_do_validation
    ! Tests implied-DO index locality validation in array constructors.
    ! A valid nested implied-DO with distinct indices is accepted; an index
    ! referenced in its own controlling triplet, and a nested index that
    ! shadows an enclosing index, are rejected.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_INTEGER
    use ast_factory_core, only: push_identifier, push_literal, push_binary_op, &
        push_array_literal
    use ast_factory, only: push_do_loop
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_implied_do_validation, only: validate_implied_do_array
    use ast_nodes_core, only: array_literal_node
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Implied-DO Index Locality Tests ==='
    print *

    call test_simple_implied_do_accepted(all_tests_passed)
    call test_nested_distinct_indices_accepted(all_tests_passed)
    call test_index_in_own_bounds_rejected(all_tests_passed)
    call test_nested_shadowing_index_rejected(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All implied-DO validation tests PASSED!'
        stop 0
    else
        print *, 'Some implied-DO validation tests FAILED!'
        stop 1
    end if

contains

    ! Build an array_literal_node with one implied-DO element [(body, var=lo,hi)].
    function build_implied_do(arena, var_name, body_index, lo_index, hi_index) &
            result(array_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: body_index, lo_index, hi_index
        integer :: array_index
        integer :: do_index
        integer :: body(1), elems(1)

        body(1) = body_index
        do_index = push_do_loop(arena, var_name, lo_index, hi_index, &
            body_indices=body, line=1, column=1)
        elems(1) = do_index
        array_index = push_array_literal(arena, elems, line=1, column=1, &
            syntax_style='implied_do')
    end function build_implied_do

    function get_array_node(arena, array_index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: array_index
        type(array_literal_node) :: node

        select type (n => arena%entries(array_index)%node)
            type is (array_literal_node)
            node = n
        end select
    end function get_array_node

    subroutine test_simple_implied_do_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: body, lo, hi, array_index

        print *, 'Testing simple implied-DO [(i, i=1,3)] (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        body = push_identifier(arena, 'i', line=1, column=2)
        lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=5)
        hi = push_literal(arena, '3', LITERAL_INTEGER, line=1, column=7)
        array_index = build_implied_do(arena, 'i', body, lo, hi)

        call validate_implied_do_array(arena, get_array_node(arena, array_index), &
            errors)

        if (errors%has_errors()) then
            print *, '  FAIL: valid implied-DO incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_simple_implied_do_accepted

    subroutine test_nested_distinct_indices_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: inner_body, inner_lo, inner_hi, inner_array
        integer :: outer_lo, outer_hi, outer_do, elems(1), outer_array

        print *, 'Testing nested distinct indices [((i+j, j=1,4), i=1,3)] ...'
        arena = create_ast_arena()
        errors = create_error_collection()
        inner_body = push_binary_op(arena, &
            push_identifier(arena, 'i', line=1, column=2), &
            push_identifier(arena, 'j', line=1, column=4), &
            '+', line=1, column=3)
        inner_lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=8)
        inner_hi = push_literal(arena, '4', LITERAL_INTEGER, line=1, column=10)
        inner_array = build_implied_do(arena, 'j', inner_body, inner_lo, inner_hi)

        outer_lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=14)
        outer_hi = push_literal(arena, '3', LITERAL_INTEGER, line=1, column=16)
        outer_do = push_do_loop(arena, 'i', outer_lo, outer_hi, &
            body_indices=[inner_array], line=1, column=1)
        elems(1) = outer_do
        outer_array = push_array_literal(arena, elems, line=1, column=1, &
            syntax_style='implied_do')

        call validate_implied_do_array(arena, &
            get_array_node(arena, outer_array), errors)

        if (errors%has_errors()) then
            print *, '  FAIL: nested distinct indices incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_nested_distinct_indices_accepted

    subroutine test_index_in_own_bounds_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: body, lo, hi, array_index

        print *, 'Testing index in own bounds [(i, i=1,i)] (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        body = push_identifier(arena, 'i', line=1, column=2)
        lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=5)
        hi = push_identifier(arena, 'i', line=1, column=7)
        array_index = build_implied_do(arena, 'i', body, lo, hi)

        call validate_implied_do_array(arena, get_array_node(arena, array_index), &
            errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: out-of-scope index reference was not rejected'
            passed = .false.
        end if
    end subroutine test_index_in_own_bounds_rejected

    subroutine test_nested_shadowing_index_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: inner_body, inner_lo, inner_hi, inner_array
        integer :: outer_lo, outer_hi, outer_do, elems(1), outer_array

        print *, 'Testing nested shadowing index [((i, i=1,3), i=1,3)] (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        inner_body = push_identifier(arena, 'i', line=1, column=3)
        inner_lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=6)
        inner_hi = push_literal(arena, '3', LITERAL_INTEGER, line=1, column=8)
        inner_array = build_implied_do(arena, 'i', inner_body, inner_lo, inner_hi)

        outer_lo = push_literal(arena, '1', LITERAL_INTEGER, line=1, column=12)
        outer_hi = push_literal(arena, '3', LITERAL_INTEGER, line=1, column=14)
        outer_do = push_do_loop(arena, 'i', outer_lo, outer_hi, &
            body_indices=[inner_array], line=1, column=1)
        elems(1) = outer_do
        outer_array = push_array_literal(arena, elems, line=1, column=1, &
            syntax_style='implied_do')

        call validate_implied_do_array(arena, &
            get_array_node(arena, outer_array), errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: shadowing nested index was not rejected'
            passed = .false.
        end if
    end subroutine test_nested_shadowing_index_rejected

end program test_implied_do_validation
