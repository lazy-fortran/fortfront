program test_procedure_actual_mapping_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_actual_argument_query_t, &
        query_procedure_actual_argument, get_subroutine_call_name
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_actual_argument_query_t) :: query
    character(len=:), allocatable :: source, call_name, error_msg
    integer :: i, direct_count, context_count, reassigned_count

    call read_example('examples/f90/procedure_actual_mapping_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'procedure actual mapping fixture was rejected: '// &
        trim(result%diagnostic_text))

    direct_count = 0
    context_count = 0
    reassigned_count = 0
    do i = 1, result%arena%size
        call get_subroutine_call_name(result%arena, i, call_name, error_msg)
        if (len_trim(error_msg) > 0 .or. trim(call_name) /= 'apply') cycle

        query = query_procedure_actual_argument(result%arena, i, 'operation')
        call require(query%found, 'apply call lost procedure actual mapping')
        call require(query%call_node_index == i .and. &
            trim(query%formal_name) == 'operation' .and. &
            query%formal_node_index > 0 .and. query%actual_node_index > 0 .and. &
            query%actual_value_node_index == query%actual_node_index, &
            'formal/actual node identity was not preserved')

        select case (trim(query%actual_name))
        case ('increment')
            direct_count = direct_count + 1
            call require(query%is_resolved .and. .not. query%is_refused .and. &
                .not. query%is_unresolved, 'direct procedure actual was not resolved')
            call require(trim(query%procedure_name) == 'increment' .and. &
                query%target_procedure_index > 0 .and. &
                query%target_binding_node_index == query%target_procedure_index, &
                'direct procedure target identity is incomplete')
            call require(query%signature%found .and. &
                trim(query%signature%procedure_name) == 'increment' .and. &
                query%signature%dummy_count == 1 .and. &
                trim(query%signature%dummies(1)%name) == 'x' .and. &
                query%signature%dummies(1)%rank_known .and. &
                query%signature%dummies(1)%rank == 0, &
                'direct procedure signature facts are incomplete')
        case ('callback')
            if (query%has_reassignment) then
                reassigned_count = reassigned_count + 1
                call require(query%is_refused .and. query%is_unresolved .and. &
                    query%has_contextual_target .and. &
                    query%has_ambiguous_target, &
                    'reassigned callback was not refused explicitly')
            else
                context_count = context_count + 1
                call require(query%is_refused .and. query%is_unresolved .and. &
                    query%has_contextual_target .and. &
                    .not. query%is_resolved .and. .not. query%signature%found, &
                    'contextual callback target was guessed')
            end if
        case default
            call require(.false., 'unexpected procedure actual name')
        end select
    end do

    call require(direct_count == 1 .and. context_count == 1 .and. &
        reassigned_count == 2, &
        'direct, contextual, and reassigned callback boundaries are incomplete')

    query = query_procedure_actual_argument(result%arena, -1, 'operation')
    call require(.not. query%found .and. .not. query%is_resolved, &
        'invalid call node received procedure actual facts')
    print *, 'PASS: bounded procedure actual/formal mapping query contract'

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

end program test_procedure_actual_mapping_query
