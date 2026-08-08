program test_procedure_callback_flow
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_callback_flow_query_t, &
        query_procedure_callback_flow, get_subroutine_call_name
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_callback_flow_query_t) :: query
    character(len=:), allocatable :: source, call_name, error_msg
    integer :: i, found

    call read_example('examples/f90/procedure_callback_flow.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'callback flow fixture was rejected')

    found = 0
    do i = 1, result%arena%size
        query = query_procedure_callback_flow(result%arena, i)
        if (query%found) then
            found = found + 1
            call get_subroutine_call_name(result%arena, i, call_name, error_msg)
            call require(len_trim(error_msg) == 0 .and. trim(call_name) == &
                'callback', 'the proof did not preserve call identity')
            call require(query%pointer_node_index == i .and. &
                query%call_node_index == i .and. query%call_pointer_node_index == i, &
                'pointer/call identity is not exact')
            call require(query%if_node_index > 0 .and. &
                query%merge_boundary_node_index == i .and. &
                query%then_entry_node_index > 0 .and. query%then_exit_node_index > 0 .and. &
                query%else_entry_node_index > 0 .and. query%else_exit_node_index > 0, &
                'branch or merge boundaries are incomplete')
            call require(trim(query%pointer_name) == 'callback' .and. &
                query%pointer_declaration_index > 0 .and. &
                size(query%targets) == 2, 'pointer or target-set identity is incomplete')
            call require(trim(query%targets(1)%procedure_name) == 'left_target' .and. &
                trim(query%targets(2)%procedure_name) == 'right_target', &
                'target ordering is not source ordering')
            call require(query%targets(1)%is_resolved .and. &
                query%targets(2)%is_resolved .and. &
                query%targets(1)%is_signature_compatible .and. &
                query%targets(2)%is_signature_compatible .and. &
                query%targets(1)%signature%found .and. &
                query%targets(2)%signature%found, 'signature facts are incomplete')
            call require(.not. query%is_unresolved .and. .not. query%is_refused, &
                'accepted proof carries refusal state')
        end if
    end do
    call require(found == 1, 'independent callback flow oracle found wrong count')

    query = query_procedure_callback_flow(result%arena, -1)
    call require(.not. query%found .and. .not. query%is_unresolved, &
        'invalid node received callback flow facts')
    print *, 'PASS: procedure callback flow query contract'

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

end program test_procedure_callback_flow
