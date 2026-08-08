program test_select_type_dispatch_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, &
        control_statement_query_t, query_control_statement, &
        CONTROL_SELECT_TYPE, type_bound_call_query_t, query_type_bound_call, &
        select_type_dispatch_query_t, query_select_type_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(type_bound_call_query_t) :: call_facts
    type(select_type_dispatch_query_t) :: dispatch
    character(len=:), allocatable :: source
    integer :: i, j, k, select_count, call_count
    logical :: saw_resolved, saw_default, saw_deferred, saw_generic
    logical :: saw_ambiguous, saw_incompatible, saw_nested, saw_dynamic
    logical :: saw_array, saw_ownership, saw_unresolved

    call read_example('examples/f90/select_type_dispatch_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'SELECT TYPE dispatch fixture did not parse')

    select_count = 0
    call_count = 0
    saw_resolved = .false.
    saw_default = .false.
    saw_deferred = .false.
    saw_generic = .false.
    saw_ambiguous = .false.
    saw_incompatible = .false.
    saw_nested = .false.
    saw_dynamic = .false.
    saw_array = .false.
    saw_ownership = .false.
    saw_unresolved = .false.

    ! The expected facts below are an independent oracle defined by the
    ! fixture's source contract. They are not inferred from the query result.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        control = query_control_statement(result%arena, i)
        call require(control%found .and. &
            control%statement_kind == CONTROL_SELECT_TYPE, &
            'SELECT TYPE control facts were not exposed')
        do j = 1, size(control%type_arms)
            do k = 1, result%arena%size
                if (.not. result%arena%has_node_at(k)) cycle
                call_facts = query_type_bound_call(result%arena, k)
                if (call_facts%call_node_index /= k) cycle
                dispatch = query_select_type_dispatch(result%arena, &
                    control%type_arms(j)%arm_node_index, k)
                if (dispatch%select_type_node_index /= i) cycle
                if (dispatch%is_resolved) then
                    saw_resolved = .true.
                    call check_resolved(dispatch)
                end if
                saw_default = saw_default .or. dispatch%is_class_default
                saw_deferred = saw_deferred .or. dispatch%is_deferred_binding
                saw_generic = saw_generic .or. dispatch%is_generic_binding
                saw_ambiguous = saw_ambiguous .or. dispatch%is_ambiguous_target
                saw_incompatible = saw_incompatible .or. &
                    dispatch%is_incompatible_pass
                saw_nested = saw_nested .or. dispatch%is_nested
                saw_dynamic = saw_dynamic .or. dispatch%is_dynamic_receiver
                saw_array = saw_array .or. dispatch%is_array_receiver
                saw_ownership = saw_ownership .or. &
                    dispatch%is_ownership_changing
                saw_unresolved = saw_unresolved .or. dispatch%is_unresolved
                call_count = call_count + 1
            end do
        end do
    end do

    call require(select_count == 10, 'unexpected SELECT TYPE construct count')
    call require(call_count > 0, 'type-bound call cases were not visited')
    call require(saw_resolved, 'concrete SELECT TYPE dispatch was not resolved')
    call require(saw_default .and. saw_deferred .and. saw_generic, &
        'deferred, generic, or CLASS DEFAULT boundary was not retained')
    call require(saw_ambiguous .and. saw_incompatible .and. saw_unresolved, &
        'ambiguous, incompatible, or unresolved boundary was not retained')
    call require(saw_nested .and. saw_dynamic .and. saw_array .and. &
        saw_ownership, 'structural and ownership refusal boundaries were lost')

    dispatch = query_select_type_dispatch(result%arena, 0, 0)
    call require(.not. dispatch%found .and. dispatch%is_unresolved .and. &
        dispatch%is_refused, 'invalid dispatch query was not refused')

    print *, 'PASS: SELECT TYPE concrete dispatch facts contract'

contains

    include '../common/read_example.inc'

    subroutine check_resolved(query)
        type(select_type_dispatch_query_t), intent(in) :: query

        call require(query%found .and. query%is_type_is, &
            'resolved arm identity is wrong')
        call require(trim(query%selector_name) == 'object' .and. &
            trim(query%guard_kind) == 'type_is' .and. &
            trim(query%concrete_type_name) == 'child_t', &
            'selector or concrete guard identity is wrong')
        call require(trim(query%binding_name) == 'run' .and. &
            trim(query%implementation) == 'child_run', &
            'binding or implementation identity is wrong')
        call require(trim(query%declaring_type_name) == 'child_t' .and. &
            .not. query%is_inherited .and. query%declaring_type_index > 0, &
            'declaring and inherited metadata is wrong')
        call require(query%pass_arg .and. .not. query%is_nopass .and. &
            trim(query%pass_name) == 'self' .and. &
            trim(query%implementation_pass_name) == 'self' .and. &
            query%implementation_pass_position == 1, &
            'PASS metadata is wrong')
        call require(query%signature%found .and. &
            query%signature%dummy_count == 2 .and. &
            trim(query%signature%dummies(1)%name) == 'self' .and. &
            trim(query%signature%dummies(2)%name) == 'amount', &
            'ordered implementation signature is wrong')
        call require(query%arm_entry_node_index == query%call_node_index .and. &
            query%arm_exit_node_index == query%call_node_index .and. &
            query%call_source_line > 0 .and. query%arm_source_line > 0, &
            'arm or call boundaries are wrong')
        call require(.not. query%is_refused .and. &
            .not. query%is_unresolved .and. query%is_binding_resolved, &
            'resolved dispatch retained a refusal')
    end subroutine check_resolved

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_select_type_dispatch_facts
