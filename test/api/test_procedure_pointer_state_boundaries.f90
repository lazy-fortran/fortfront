program test_procedure_pointer_state_boundaries
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_pointer_state_query_t, &
        query_procedure_pointer_state
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_pointer_state_query_t) :: query
    character(len=:), allocatable :: source, executable
    integer :: i, status
    integer :: local_known_count
    logical :: saw_local_known, saw_dummy_alias, saw_host_associated_alias
    logical :: saw_host_nullify_alias, saw_global_refusal

    call read_example('examples/f90/procedure_pointer_state_boundaries.f90', &
        source)
    executable = test_executable_path('fortfront_procedure_pointer_state_boundaries')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), &
        'procedure-pointer boundary fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/procedure_pointer_state_boundaries.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected boundary fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=status)
    call require(status == 0, &
        'GNU runtime oracle rejected boundary fixture behavior')
    call test_remove_file(executable)

    saw_local_known = .false.
    local_known_count = 0
    saw_dummy_alias = .false.
    saw_host_associated_alias = .false.
    saw_host_nullify_alias = .false.
    saw_global_refusal = .false.
    do i = 1, result%arena%size
        query = query_procedure_pointer_state(result%arena, i)
        if (.not. query%found) cycle
        if (query%is_associated_test) then
            select case (trim(query%pointer_name))
            case ('local_callback')
                if (query%state_known) then
                    saw_local_known = .true.
                    local_known_count = local_known_count + 1
                    call require(query%is_associated .and. &
                        .not. query%is_refused .and. .not. query%is_unresolved .and. &
                        .not. query%has_control_flow_boundary, &
                        'same-scope local callback state is not known')
                else
                    saw_host_associated_alias = .true.
                    call require(query%has_alias .and. query%is_refused .and. &
                        query%is_unresolved .and. .not. query%state_known, &
                        'host-associated callback was not refused as an alias')
                end if
            case ('dummy_callback')
                saw_dummy_alias = .true.
                call require(query%has_alias .and. query%is_refused .and. &
                    query%is_unresolved .and. .not. query%state_known, &
                    'procedure-pointer dummy was not refused as an alias')
            case ('global_callback')
                saw_global_refusal = .true.
                call require(query%has_global_mutable_state .and. &
                    query%is_refused .and. query%is_unresolved .and. &
                    .not. query%state_known, &
                    'module callback was not refused as global state')
            case default
                call require(.false., 'unexpected boundary ASSOCIATED operand')
            end select
        else if (query%is_nullify) then
            call require(trim(query%pointer_name) == 'local_callback', &
                'unexpected boundary NULLIFY operand')
            saw_host_nullify_alias = .true.
            call require(query%has_alias .and. query%is_refused .and. &
                query%is_unresolved .and. .not. query%state_known, &
                'host-associated NULLIFY was not refused as an alias')
        end if
    end do

    call require(local_known_count == 2 .and. saw_local_known .and. &
        saw_dummy_alias .and. &
        saw_host_associated_alias .and. &
        saw_host_nullify_alias .and. saw_global_refusal, &
        'procedure-pointer alias/global boundaries are incomplete')
    print *, 'PASS: procedure-pointer alias/global refusal boundaries'

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

end program test_procedure_pointer_state_boundaries
