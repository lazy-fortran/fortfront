program test_procedure_pointer_state_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_pointer_state_query_t, &
        query_procedure_pointer_state
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_pointer_state_query_t) :: query
    character(len=:), allocatable :: source, executable
    integer :: i, associated_count, nullify_count, status
    logical :: saw_active, saw_cleared, saw_reassigned, saw_branched
    logical :: saw_nullify, saw_data_refusal, saw_second_argument

    call read_example('examples/f90/procedure_pointer_association_query.f90', &
        source)
    executable = test_executable_path('fortfront_procedure_pointer_state_oracle')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), &
        'procedure-pointer association fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/procedure_pointer_association_query.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected association fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=status)
    call require(status == 0, &
        'GNU runtime oracle rejected association fixture behavior')
    call test_remove_file(executable)

    associated_count = 0
    nullify_count = 0
    saw_active = .false.
    saw_cleared = .false.
    saw_reassigned = .false.
    saw_branched = .false.
    saw_nullify = .false.
    saw_data_refusal = .false.
    saw_second_argument = .false.
    do i = 1, result%arena%size
        query = query_procedure_pointer_state(result%arena, i)
        if (.not. query%found) cycle
        if (query%is_associated_test) then
            associated_count = associated_count + 1
            call require(.not. query%is_nullify, &
                'ASSOCIATED observation was also marked NULLIFY')
            select case (trim(query%pointer_name))
            case ('active_callback')
                saw_active = .true.
                call require(query%state_known .and. query%is_associated .and. &
                    .not. query%is_refused .and. .not. query%is_unresolved, &
                    'direct associated callback was not proved true')
                call require(query%assignment_node_index > 0, &
                    'direct association lost assignment identity')
            case ('cleared_callback')
                saw_cleared = .true.
                call require(query%state_known .and. .not. query%is_associated, &
                    'NULLIFY callback was not proved disassociated')
                call require(query%has_nullify .and. &
                    query%nullify_node_index > 0, &
                    'NULLIFY state lost its source identity')
            case ('reassigned_callback')
                saw_reassigned = .true.
                call require(.not. query%state_known .and. query%is_refused .and. &
                    query%is_unresolved .and. query%has_reassignment, &
                    'reassigned callback was not explicitly refused')
            case ('branched_callback')
                saw_branched = .true.
                call require(.not. query%state_known .and. query%is_refused .and. &
                    query%is_unresolved .and. query%has_flow_sensitive_state, &
                    'branch-local callback was not explicitly refused')
            case ('data_pointer')
                saw_data_refusal = .true.
                call require(.not. query%state_known .and. query%is_refused .and. &
                    query%is_unresolved .and. query%has_non_procedure_pointer, &
                    'ordinary data pointer was not explicitly refused')
            case default
                if (query%has_second_argument) then
                    saw_second_argument = .true.
                    call require(query%has_invalid_arity .and. query%is_refused .and. &
                        query%is_unresolved .and. .not. query%state_known, &
                        'two-argument ASSOCIATED was not explicitly refused')
                else
                    call require(.false., 'unexpected ASSOCIATED pointer')
                end if
            end select
        else if (query%is_nullify) then
            nullify_count = nullify_count + 1
            call require(query%state_known .and. .not. query%is_associated .and. &
                .not. query%is_refused .and. .not. query%is_unresolved, &
                'direct NULLIFY state fact is incomplete')
            if (trim(query%pointer_name) == 'cleared_callback') saw_nullify = .true.
        end if
    end do

    call require(associated_count == 6 .and. saw_active .and. saw_cleared .and. &
        saw_reassigned .and. saw_branched .and. saw_data_refusal .and. &
        saw_second_argument, &
        'ASSOCIATED state cases are incomplete')
    call require(nullify_count == 2 .and. saw_nullify, &
        'NULLIFY state cases are incomplete')

    print *, 'PASS: bounded procedure-pointer ASSOCIATED/NULLIFY query contract'

contains

    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_procedure_pointer_state_query
