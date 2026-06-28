program test_abstract_validation
    ! Tests that a concrete derived type extending an ABSTRACT type must
    ! implement every inherited DEFERRED type-bound procedure (F2003 C464).
    ! An overriding child is accepted, a non-overriding child is rejected, and a
    ! still-ABSTRACT child is accepted.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: derived_type_node
    use ast_factory_declarations, only: push_derived_type, push_type_binding
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_abstract_validation, only: validate_abstract_extension, &
        is_abstract_type
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== ABSTRACT Extension Validation Tests ==='
    print *

    call test_overriding_child_accepted(all_tests_passed)
    call test_non_overriding_child_rejected(all_tests_passed)
    call test_abstract_child_accepted(all_tests_passed)
    call test_is_abstract_type_classification(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All ABSTRACT validation tests PASSED!'
        stop 0
    else
        print *, 'Some ABSTRACT validation tests FAILED!'
        stop 1
    end if

contains

    ! Push an ABSTRACT base type with one DEFERRED binding named "area".
    function push_abstract_base(arena) result(base_index)
        type(ast_arena_t), intent(inout) :: arena
        integer :: base_index
        integer :: bindings(1)

        bindings(1) = push_type_binding(arena, 'area', is_deferred=.true., &
            interface_name='area_iface', line=1, &
            column=1)
        base_index = push_derived_type(arena, 'shape_t', &
            attribute_clause='abstract', &
            binding_indices=bindings, line=2, column=1)
    end function push_abstract_base

    subroutine test_overriding_child_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: base_index, child_index, bindings(1)

        print *, 'Testing concrete child overriding deferred binding (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        base_index = push_abstract_base(arena)
        bindings(1) = push_type_binding(arena, 'area', implementation='circle_area', &
            line=10, column=1)
        child_index = push_derived_type(arena, 'circle_t', &
            extends_parent='shape_t', &
            binding_indices=bindings, line=11, column=1)

        call run_validation(arena, child_index, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: overriding child incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_overriding_child_accepted

    subroutine test_non_overriding_child_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: base_index, child_index, comps(1)

        print *, 'Testing concrete child not overriding deferred binding (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        base_index = push_abstract_base(arena)
        comps(1) = push_type_binding(arena, 'describe', implementation='circle_desc', &
            line=10, column=1)
        child_index = push_derived_type(arena, 'circle_t', &
            extends_parent='shape_t', &
            binding_indices=comps, line=11, column=1)

        call run_validation(arena, child_index, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: missing deferred implementation was not rejected'
            passed = .false.
        end if
    end subroutine test_non_overriding_child_rejected

    subroutine test_abstract_child_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: base_index, child_index

        print *, 'Testing still-ABSTRACT child without override (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        base_index = push_abstract_base(arena)
        child_index = push_derived_type(arena, 'polygon_t', &
            extends_parent='shape_t', &
            attribute_clause='abstract', &
            line=11, column=1)

        call run_validation(arena, child_index, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: abstract child incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_abstract_child_accepted

    subroutine test_is_abstract_type_classification(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        integer :: abstract_index, plain_index
        logical :: ok

        print *, 'Testing is_abstract_type classification...'
        arena = create_ast_arena()
        ok = .true.

        abstract_index = push_derived_type(arena, 'shape_t', &
            attribute_clause='ABSTRACT, public', &
            line=1, column=1)
        plain_index = push_derived_type(arena, 'point_t', line=2, column=1)

        if (.not. node_is_abstract(arena, abstract_index)) ok = .false.
        if (node_is_abstract(arena, plain_index)) ok = .false.

        if (ok) then
            print *, '  PASS'
        else
            print *, '  FAIL: is_abstract_type misclassified a type header'
            passed = .false.
        end if
    end subroutine test_is_abstract_type_classification

    ! Fetch the derived type at type_index and validate it.
    subroutine run_validation(arena, type_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        type(error_collection_t), intent(inout) :: errors

        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            call validate_abstract_extension(arena, node, errors)
        end select
    end subroutine run_validation

    ! Classify the derived type at type_index via is_abstract_type.
    function node_is_abstract(arena, type_index) result(is_abstract)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        logical :: is_abstract

        is_abstract = .false.
        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            is_abstract = is_abstract_type(node)
        end select
    end function node_is_abstract

end program test_abstract_validation
