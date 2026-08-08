program test_procedure_callback_missing_assignment
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_callback_flow_query_t, &
        query_procedure_callback_flow
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_callback_flow_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, missing_count, status

    call read_example('examples/f90/procedure_callback_flow_missing_assignment.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'missing-assignment fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/procedure_callback_flow_missing_assignment.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the missing-assignment fixture')

    missing_count = 0
    do i = 1, result%arena%size
        query = query_procedure_callback_flow(result%arena, i)
        if (.not. query%has_missing_assignment) cycle
        missing_count = missing_count + 1
        call require(.not. query%found .and. query%is_unresolved .and. &
            query%is_refused .and. .not. query%has_reassignment, &
            'missing callback assignment was not classified precisely')
    end do

    call require(missing_count == 1, &
        'missing callback assignment refusal was not observed exactly once')
    print *, 'PASS: procedure callback missing-assignment refusal fact'

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

end program test_procedure_callback_missing_assignment
