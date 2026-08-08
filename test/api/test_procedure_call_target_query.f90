program test_procedure_call_target_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_call_target_query_t, &
        query_procedure_call_target
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_call_target_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, resolved_calls, unresolved_calls, generic_calls
    logical :: saw_internal, saw_external
    logical :: saw_reassigned, saw_branched, saw_null, saw_nullified

    call read_example('examples/f90/procedure_call_target_query.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'procedure call target example was rejected')

    resolved_calls = 0
    unresolved_calls = 0
    saw_internal = .false.
    saw_external = .false.
    saw_reassigned = .false.
    saw_branched = .false.
    saw_null = .false.
    saw_nullified = .false.
    do i = 1, result%arena%size
        query = query_procedure_call_target(result%arena, i)
        if (query%found) then
            resolved_calls = resolved_calls + 1
            call require(query%is_resolved .and. .not. query%is_unresolved, &
                'resolved call target state is incorrect')
            call require(query%call_node_index == i, &
                'call identity was not preserved')
            call require(query%pointer_node_index == i .and. &
                query%pointer_declaration_index > 0, &
                'pointer identity was not preserved')
            call require(query%assignment_node_index > 0 .and. &
                query%target_node_index > 0 .and. query%scope_node_index > 0, &
                'assignment or target identity is missing')
            call require(query%target_procedure_index > 0 .or. &
                query%target_declaration_index > 0, &
                'resolved target has no procedure identity')
            call require(query%target_binding_node_index > 0 .and. &
                len_trim(query%target_binding_name) > 0, &
                'resolved target has no binding identity')
            select case (trim(query%procedure_name))
            case ('internal_scale', 'internal_action')
                saw_internal = .true.
            case ('external_scale')
                saw_external = .true.
            case default
                call require(.false., 'unexpected resolved procedure')
            end select
        else if (query%is_unresolved) then
            unresolved_calls = unresolved_calls + 1
            call require(query%call_node_index == i .and. &
                query%pointer_node_index == i .and. &
                query%pointer_declaration_index > 0, &
                'unresolved call did not preserve call and pointer identity')
            call require(.not. query%is_resolved .and. &
                query%assignment_node_index == 0, &
                'unresolved call exposed a target fact')
            select case (trim(query%pointer_name))
            case ('reassigned_callback')
                saw_reassigned = .true.
            case ('branched_callback')
                saw_branched = .true.
            case ('null_callback')
                saw_null = .true.
            case ('nullified_callback')
                saw_nullified = .true.
            case default
                call require(.false., 'unexpected unresolved callback')
            end select
        end if
    end do

    call require(resolved_calls == 3 .and. saw_internal .and. saw_external, &
        'resolved internal and external callback calls are incomplete')
    call require(unresolved_calls == 4 .and. saw_reassigned .and. &
        saw_branched .and. saw_null .and. saw_nullified, &
        'branch, reassignment, NULL, or NULLIFY callbacks were not refused')

    call read_example('examples/f90/generic_resolution_query.f90', source)
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'generic example was rejected')
    generic_calls = 0
    do i = 1, result%arena%size
        query = query_procedure_call_target(result%arena, i)
        if (query%found .or. query%is_unresolved) generic_calls = generic_calls + 1
    end do
    call require(generic_calls == 0, 'generic calls were treated as callbacks')

    print *, 'PASS: bounded procedure-pointer call target query contract'

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

end program test_procedure_call_target_query
