program test_issue_2829_array_bounds_validation
    ! Issue #2829 (b): validate_array_bounds now checks compile-time-constant
    ! slice bounds instead of always returning true. A backwards constant range
    ! and a zero stride are rejected; valid and runtime ranges pass.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: literal_node, identifier_node
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node
    use semantic_validation_utils, only: validate_array_bounds
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_valid_constant_range()
    call test_backwards_constant_range()
    call test_zero_stride()
    call test_runtime_bounds_pass()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", &
        test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    ! Push an integer literal node with the given value text, return its index.
    integer function push_int_literal(arena, text) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: text
        type(literal_node) :: lit

        lit%value = text
        lit%literal_type = "integer"
        call arena%push(lit, "literal", 0)
        idx = arena%size
    end function push_int_literal

    ! Push an array_bounds_node with given lower/upper/stride expression indices.
    integer function push_bounds(arena, lower, upper, stride) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: lower, upper, stride
        type(array_bounds_node) :: bounds

        bounds%lower_bound_index = lower
        bounds%upper_bound_index = upper
        bounds%stride_index = stride
        call arena%push(bounds, "array_bounds", 0)
        idx = arena%size
    end function push_bounds

    ! Push a single-dimension slice referencing one bounds node, return index.
    integer function push_slice(arena, bounds_idx) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: bounds_idx
        type(array_slice_node) :: slice

        slice%array_index = 1
        slice%bounds_indices(1) = bounds_idx
        slice%num_dimensions = 1
        call arena%push(slice, "array_slice", 0)
        idx = arena%size
    end function push_slice

    logical function slice_valid(arena, slice_idx) result(ok)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: slice_idx

        ok = .true.
        select type (node => arena%entries(slice_idx)%node)
        type is (array_slice_node)
            call validate_array_bounds(arena, node, ok)
        end select
    end function slice_valid

    subroutine test_valid_constant_range()
        type(ast_arena_t) :: arena
        integer :: lo, hi, b, s

        test_count = test_count + 1
        arena = create_ast_arena()
        lo = push_int_literal(arena, "1")
        hi = push_int_literal(arena, "5")
        b = push_bounds(arena, lo, hi, -1)
        s = push_slice(arena, b)
        if (slice_valid(arena, s)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: 1:5 is valid"
        else
            write (*, '(A)') "FAIL: 1:5 should be valid"
        end if
    end subroutine test_valid_constant_range

    subroutine test_backwards_constant_range()
        type(ast_arena_t) :: arena
        integer :: lo, hi, b, s

        test_count = test_count + 1
        arena = create_ast_arena()
        lo = push_int_literal(arena, "5")
        hi = push_int_literal(arena, "1")
        b = push_bounds(arena, lo, hi, -1)
        s = push_slice(arena, b)
        if (.not. slice_valid(arena, s)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: 5:1 (positive stride) is rejected"
        else
            write (*, '(A)') "FAIL: 5:1 should be rejected"
        end if
    end subroutine test_backwards_constant_range

    subroutine test_zero_stride()
        type(ast_arena_t) :: arena
        integer :: lo, hi, st, b, s

        test_count = test_count + 1
        arena = create_ast_arena()
        lo = push_int_literal(arena, "1")
        hi = push_int_literal(arena, "5")
        st = push_int_literal(arena, "0")
        b = push_bounds(arena, lo, hi, st)
        s = push_slice(arena, b)
        if (.not. slice_valid(arena, s)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: zero stride is rejected"
        else
            write (*, '(A)') "FAIL: zero stride should be rejected"
        end if
    end subroutine test_zero_stride

    subroutine test_runtime_bounds_pass()
        type(ast_arena_t) :: arena
        type(identifier_node) :: id
        integer :: lo, hi, b, s

        test_count = test_count + 1
        arena = create_ast_arena()
        id%name = "n"
        call arena%push(id, "identifier", 0)
        lo = arena%size
        id%name = "m"
        call arena%push(id, "identifier", 0)
        hi = arena%size
        b = push_bounds(arena, lo, hi, -1)
        s = push_slice(arena, b)
        ! Non-constant bounds cannot be decided statically: must pass.
        if (slice_valid(arena, s)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: runtime bounds treated as valid"
        else
            write (*, '(A)') "FAIL: runtime bounds should pass static check"
        end if
    end subroutine test_runtime_bounds_pass

end program test_issue_2829_array_bounds_validation
