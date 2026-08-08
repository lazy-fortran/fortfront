program test_type_bound_nested_receiver_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, type_bound_call_query_t, query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(type_bound_call_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, apply_calls, reset_calls, measure_calls
    integer :: ambiguous_calls, missing_calls, polymorphic_calls

    call read_example('examples/f90/type_bound_nested_receiver_query.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'nested receiver example did not parse')

    apply_calls = 0
    reset_calls = 0
    measure_calls = 0
    ambiguous_calls = 0
    missing_calls = 0
    polymorphic_calls = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        query = query_type_bound_call(result%arena, i)
        if (.not. query%found .and. .not. query%is_unresolved) cycle
        if (trim(query%receiver_name) == 'outer%inner' .or. &
            trim(query%receiver_name) == 'outer%polymorphic_inner') then
            call require(trim(query%declared_type_name) == 'inner_t', &
                'nested receiver type was not resolved')
        end if

        select case (trim(query%binding_name))
        case ('apply')
            apply_calls = apply_calls + 1
            call require(query%found .and. query%pass_arg, &
                'nested PASS binding was not found')
            call require(trim(query%pass_name) == 'self', &
                'nested PASS name was lost')
            call require(trim(query%receiver_name) == 'outer%inner', &
                'nested PASS receiver identity was lost')
        case ('reset')
            reset_calls = reset_calls + 1
            call require(query%found .and. .not. query%pass_arg, &
                'nested NOPASS binding was not found')
            call require(len_trim(query%pass_name) == 0, &
                'nested NOPASS exposed a PASS name')
        case ('measure')
            measure_calls = measure_calls + 1
            call require(query%found .and. query%is_resolved, &
                'nested function binding was not resolved')
            call require(query%receiver_path%found, &
                'nested expression receiver path was not exposed')
            call require(query%receiver_path%node_index == &
                query%receiver_node_index, 'receiver path node identity changed')
            call require(query%receiver_path%base_node_index > 0 .and. &
                size(query%receiver_path%component_names) == 1, &
                'nested receiver component path is wrong')
            if (trim(query%receiver_name) == 'outer%inner') then
                call require(trim(query%receiver_path%component_names(1)) == &
                    'inner', 'static receiver component path is wrong')
            else
                call require(trim(query%receiver_path%component_names(1)) == &
                    'polymorphic_inner', 'polymorphic path is wrong')
                call require(size(query%dispatch_target_type_indices) == 1 .and. &
                    trim(query%dispatch_target_implementations(1)) == &
                    'child_measure', 'polymorphic dispatch was not explicit')
            end if
            call require(size(query%receiver_path%component_node_indices) == 1 .and. &
                query%receiver_path%component_node_indices(1) > 0, &
                'nested receiver component node identity is missing')
            if (trim(query%receiver_name) == 'outer%polymorphic_inner') then
                polymorphic_calls = polymorphic_calls + 1
            end if
        case ('ambiguous')
            ambiguous_calls = ambiguous_calls + 1
            call require(query%found .and. query%is_generic .and. &
                query%is_ambiguous .and. .not. query%is_resolved, &
                'ambiguous nested binding was not refused')
            call require(len_trim(query%implementation) == 0 .and. &
                size(query%dispatch_target_type_indices) == 0, &
                'ambiguous nested binding exposed a target guess')
        case ('missing')
            missing_calls = missing_calls + 1
            call require(.not. query%found .and. query%is_unresolved, &
                'missing nested binding was not refused')
            call require(trim(query%declared_type_name) == 'inner_t', &
                'missing nested binding lost receiver type')
        end select
    end do

    call require(apply_calls == 1 .and. reset_calls == 1 .and. &
        measure_calls == 2 .and. ambiguous_calls == 1 .and. &
        missing_calls == 1 .and. polymorphic_calls == 1, &
        'nested receiver cases were not enumerated exactly once')

    print *, 'PASS: nested type-bound receiver query contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_type_bound_nested_receiver_query
