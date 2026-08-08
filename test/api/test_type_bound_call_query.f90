program test_type_bound_call_query
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
    integer :: i, base_index, child_index
    integer :: run_calls, inherited_calls, generic_calls, unresolved_calls

    call read_example('examples/f90/type_bound_call_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'type-bound call example did not parse')

    base_index = 0
    child_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        if (trim(derived%name) == 'base_t') base_index = i
        if (trim(derived%name) == 'child_t') child_index = i
    end do
    call require(base_index > 0, 'base type was not found')
    call require(child_index > 0, 'child type was not found')

    run_calls = 0
    inherited_calls = 0
    generic_calls = 0
    unresolved_calls = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        query = query_type_bound_call(result%arena, i)
        if (.not. query%found .and. .not. query%is_unresolved) cycle
        if (trim(query%binding_name) == 'run' .and. &
            trim(query%declared_type_name) == 'base_t') then
            run_calls = run_calls + 1
            call require(query%found, 'base RUN binding was not found')
            call require(trim(query%receiver_name) == 'self', &
                'call receiver name was not reported')
            call require(query%receiver_declaration_index > 0, &
                'call receiver declaration was not reported')
            call require(query%receiver_node_index == 0, &
                'explicit call invented a receiver AST node')
            call require(query%is_deferred, 'base RUN was not marked deferred')
            call require(.not. query%is_resolved, &
                'deferred RUN received an implementation')
            call require(trim(query%pass_name) == 'self', &
                'RUN PASS name was lost')
            call require(size(query%dispatch_target_type_indices) == 1, &
                'child RUN dispatch target was not reported')
            call require(query%dispatch_target_type_indices(1) == child_index, &
                'RUN dispatch target type is wrong')
            call require(trim(query%dispatch_target_implementations(1)) == &
                'child_run', 'RUN dispatch target implementation is wrong')
        else if (trim(query%binding_name) == 'inherited') then
            inherited_calls = inherited_calls + 1
            call require(query%found, 'inherited binding facts were lost')
            if (trim(query%declared_type_name) == 'base_t') then
                call require(.not. query%is_inherited, &
                    'local base binding was marked inherited')
            else if (trim(query%declared_type_name) == 'child_t') then
                call require(query%is_inherited, &
                    'child inherited binding was not marked inherited')
            end if
            call require(.not. query%pass_arg, 'NOPASS metadata was lost')
            call require(trim(query%implementation) == 'base_inherited', &
                'inherited implementation is wrong')
        else if (trim(query%binding_name) == 'ambiguous') then
            generic_calls = generic_calls + 1
            call require(query%found .and. query%is_generic, &
                'generic binding facts were lost')
            call require(query%is_ambiguous .and. .not. query%is_resolved, &
                'ambiguous generic was resolved')
            call require(len_trim(query%implementation) == 0, &
                'ambiguous generic received an implementation guess')
            call require(size(query%dispatch_target_type_indices) == 0, &
                'ambiguous generic exposed a target guess')
        else if (trim(query%binding_name) == 'missing') then
            unresolved_calls = unresolved_calls + 1
            call require(.not. query%found .and. query%is_unresolved, &
                'missing binding was not refused')
            call require(trim(query%declared_type_name) == 'base_t', &
                'unresolved call lost its receiver type')
        end if
    end do

    query = query_type_bound_call(result%arena, find_child_run_call(result))
    call require(query%found .and. query%is_resolved, &
        'child override call was not resolved')
    call require(query%declared_type_index == child_index, &
        'child declared receiver type is wrong')
    call require(trim(query%implementation) == 'child_run', &
        'child implementation is wrong')
    call require(.not. query%is_inherited, 'child override was marked inherited')
    call require(run_calls == 1 .and. &
        generic_calls == 1 .and. unresolved_calls == 1 .and. &
        inherited_calls == 2, &
        'base call-site cases were not enumerated exactly once')

    print *, 'PASS: type-bound call query contract'

contains

    include '../common/read_example.inc'

    integer function find_child_run_call(frontend_result) result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        type(type_bound_call_query_t) :: candidate
        integer :: node

        node_index = 0
        do node = 1, frontend_result%arena%size
            if (.not. frontend_result%arena%has_node_at(node)) cycle
            candidate = query_type_bound_call(frontend_result%arena, node)
            if (.not. candidate%found) cycle
            if (trim(candidate%declared_type_name) /= 'child_t') cycle
            if (trim(candidate%binding_name) /= 'run') cycle
            node_index = node
            return
        end do
    end function find_child_run_call

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) call fail(message)
    end subroutine require

    subroutine fail(message)
        character(len=*), intent(in) :: message
        print *, 'FAIL: ', trim(message)
        error stop 1
    end subroutine fail

end program test_type_bound_call_query
