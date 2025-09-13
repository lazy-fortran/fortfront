program test_range_subscript_core
    use ast_nodes_core, only: range_subscript_node, create_range_subscript
    implicit none

    logical :: all_passed
    all_passed = .true.

    call test_create_defaults(all_passed)
    call test_create_full(all_passed)

    if (all_passed) then
        print *, 'range_subscript core tests passed'
    else
        print *, 'range_subscript core tests FAILED'
        stop 1
    end if

contains

    subroutine test_create_defaults(all_passed)
        logical, intent(inout) :: all_passed
        type(range_subscript_node) :: n
        logical :: pass
        pass = .true.
        n = create_range_subscript(5)
        if (n%base_expr_index /= 5) pass = .false.
        if (n%start_index /= -1) pass = .false.
        if (n%end_index /= -1) pass = .false.
        if (n%is_character_substring) pass = .false.
        if (pass) then
            print *, '  PASS: defaults'
        else
            print *, '  FAIL: defaults'
        end if
        all_passed = all_passed .and. pass
    end subroutine test_create_defaults

    subroutine test_create_full(all_passed)
        logical, intent(inout) :: all_passed
        type(range_subscript_node) :: n
        logical :: pass
        pass = .true.
        n = create_range_subscript(1, 2, 3, 10, 20)
        if (n%base_expr_index /= 1) pass = .false.
        if (n%start_index /= 2) pass = .false.
        if (n%end_index /= 3) pass = .false.
        if (n%line /= 10 .or. n%column /= 20) pass = .false.
        if (pass) then
            print *, '  PASS: full parameters'
        else
            print *, '  FAIL: full parameters'
        end if
        all_passed = all_passed .and. pass
    end subroutine test_create_full

end program test_range_subscript_core

