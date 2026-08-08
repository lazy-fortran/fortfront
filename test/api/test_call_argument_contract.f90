program test_call_argument_contract
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, call_arguments_query_t, query_call_arguments
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(call_arguments_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i
    logical :: saw_update, saw_global, saw_alias, saw_callback

    call read_example('examples/f90/call_argument_contract.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'call contract example was rejected: '// &
        trim(result%diagnostic_text))

    saw_update = .false.
    saw_global = .false.
    saw_alias = .false.
    saw_callback = .false.
    do i = 1, result%arena%size
        query = query_call_arguments(result%arena, i)
        if (.not. query%found) cycle
        select case (trim(query%procedure_name))
        case ('update')
            saw_update = .true.
            call require(.not. query%is_refused, &
                'ordinary update call was refused')
            call require(.not. query%has_global_mutable_state .and. &
                .not. query%has_unresolved_alias .and. &
                .not. query%has_procedure_callback, &
                'ordinary update call has an unexpected boundary')
            call require(size(query%arguments) == 2 .and. &
                trim(query%arguments(1)%formal_name) == 'value' .and. &
                trim(query%arguments(1)%formal_intent) == 'inout' .and. &
                query%arguments(1)%formal_rank == 0 .and. &
                query%arguments(1)%actual_rank == 0 .and. &
                query%arguments(1)%type_compatibility_known .and. &
                .not. query%arguments(1)%has_type_mismatch, &
                'formal/actual scalar facts were not preserved')
            call require(trim(query%arguments(2)%formal_name) == 'scale' .and. &
                trim(query%arguments(2)%formal_intent) == 'in', &
                'second formal intent was not preserved')
        case ('update_global')
            saw_global = .true.
            call require(query%has_global_mutable_state .and. query%is_refused, &
                'global mutable state was not refused explicitly')
        case ('update_alias')
            saw_alias = .true.
            call require(query%has_unresolved_alias .and. query%is_refused, &
                'repeated actual alias was not refused explicitly')
        case ('apply_callback')
            saw_callback = .true.
            call require(query%has_procedure_callback .and. query%is_refused, &
                'procedure callback was not refused explicitly')
        end select
    end do

    call require(saw_update .and. saw_global .and. saw_alias .and. saw_callback, &
        'call contract fixture did not expose every call class')
    print *, 'PASS: call argument contract preserves facts and refusals'

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

end program test_call_argument_contract
