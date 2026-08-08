program test_type_bound_dispatch_signature
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, type_bound_call_query_t, query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(type_bound_call_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, base_index, child_a_index, child_b_index
    integer :: run_calls, ambiguous_calls, generic_calls, unresolved_calls

    call read_example('examples/f90/type_bound_dispatch_signature.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'dispatch signature example did not parse')

    base_index = 0
    child_a_index = 0
    child_b_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        select case (trim(derived%name))
        case ('base_t')
            base_index = i
        case ('child_a_t')
            child_a_index = i
        case ('child_b_t')
            child_b_index = i
        end select
    end do
    call require(base_index > 0 .and. child_a_index > 0 .and. &
        child_b_index > 0, 'dispatch signature types are missing')

    run_calls = 0
    ambiguous_calls = 0
    generic_calls = 0
    unresolved_calls = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        query = query_type_bound_call(result%arena, i)
        if (.not. query%found .and. .not. query%is_unresolved) cycle
        if (trim(query%declared_type_name) /= 'base_t') cycle

        select case (trim(query%binding_name))
        case ('run')
            run_calls = run_calls + 1
            call require(query%found .and. query%is_deferred .and. &
                .not. query%is_resolved, 'deferred RUN facts are wrong')
            call require(size(query%dispatch_target_type_indices) == 2 .and. &
                size(query%dispatch_target_implementations) == 2, &
                'legacy dispatch target arrays changed shape')
            call require(size(query%dispatch_target_pass_names) == 2 .and. &
                size(query%dispatch_target_pass_positions) == 2 .and. &
                size(query%dispatch_target_passed_object_types) == 2 .and. &
                size(query%dispatch_target_signature_resolved) == 2, &
                'dispatch signature arrays are incomplete')
            call require_target(query, child_a_index, 'child_a_run', 'self', &
                2, 'class(child_a_t)')
            call require_target(query, child_b_index, 'child_b_run', 'obj', &
                2, 'class(child_b_t)')
        case ('ambiguous')
            ambiguous_calls = ambiguous_calls + 1
            call require_refusal(query, .true., &
                'ambiguous generic exposed a target guess')
        case ('generic')
            generic_calls = generic_calls + 1
            call require_refusal(query, .false., &
                'generic binding exposed a target guess')
        case ('unresolved')
            unresolved_calls = unresolved_calls + 1
            call require(.not. query%found .and. query%is_unresolved, &
                'unresolved binding was not refused')
            call require(size(query%dispatch_target_type_indices) == 0 .and. &
                size(query%dispatch_target_pass_names) == 0 .and. &
                size(query%dispatch_target_pass_positions) == 0 .and. &
                size(query%dispatch_target_passed_object_types) == 0 .and. &
                size(query%dispatch_target_signature_resolved) == 0, &
                'unresolved binding exposed target facts')
        end select
    end do

    call require(run_calls == 1 .and. ambiguous_calls == 1 .and. &
        generic_calls == 1 .and. unresolved_calls == 1, &
        'dispatch signature cases were not enumerated exactly once')
    print *, 'PASS: type-bound dispatch signature contract'

contains

    include '../common/read_example.inc'

    subroutine require_target(candidate, type_index, implementation, &
            pass_name, pass_position, passed_object_type)
        type(type_bound_call_query_t), intent(in) :: candidate
        integer, intent(in) :: type_index, pass_position
        character(len=*), intent(in) :: implementation, pass_name, &
            passed_object_type
        integer :: target

        target = 0
        do target = 1, size(candidate%dispatch_target_type_indices)
            if (candidate%dispatch_target_type_indices(target) == type_index) exit
        end do
        call require(target <= size(candidate%dispatch_target_type_indices), &
            'expected dispatch target type is missing')
        call require(trim(candidate%dispatch_target_implementations(target)) == &
            implementation, 'effective dispatch implementation is wrong')
        call require(trim(candidate%dispatch_target_pass_names(target)) == &
            pass_name, 'effective PASS name is wrong')
        call require(candidate%dispatch_target_pass_positions(target) == &
            pass_position, 'effective PASS position is wrong')
        call require(trim(candidate%dispatch_target_passed_object_types(target)) == &
            passed_object_type, 'passed-object declared type is wrong')
        call require(candidate%dispatch_target_signature_resolved(target), &
            'dispatch signature was not marked resolved')
    end subroutine require_target

    subroutine require_refusal(candidate, expected_ambiguous, message)
        type(type_bound_call_query_t), intent(in) :: candidate
        logical, intent(in) :: expected_ambiguous
        character(len=*), intent(in) :: message

        call require(candidate%found .and. candidate%is_generic .and. &
            candidate%is_ambiguous .eqv. expected_ambiguous .and. &
            .not. candidate%is_resolved, message)
        call require(len_trim(candidate%implementation) == 0 .and. &
            size(candidate%dispatch_target_type_indices) == 0 .and. &
            size(candidate%dispatch_target_implementations) == 0 .and. &
            size(candidate%dispatch_target_pass_names) == 0 .and. &
            size(candidate%dispatch_target_pass_positions) == 0 .and. &
            size(candidate%dispatch_target_passed_object_types) == 0 .and. &
            size(candidate%dispatch_target_signature_resolved) == 0, &
            'refused binding exposed a target guess')
    end subroutine require_refusal

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_type_bound_dispatch_signature
