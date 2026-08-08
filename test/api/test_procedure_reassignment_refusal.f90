program test_procedure_reassignment_refusal
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_call_target_query_t, &
        query_procedure_call_target
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_call_target_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, reassigned_count, nullified_count, status

    call read_example('examples/f90/procedure_call_target_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'procedure-pointer refusal fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/procedure_call_target_query.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the refusal fixture')

    reassigned_count = 0
    nullified_count = 0
    do i = 1, result%arena%size
        query = query_procedure_call_target(result%arena, i)
        if (.not. query%is_unresolved) cycle
        select case (trim(query%pointer_name))
        case ('reassigned_callback')
            reassigned_count = reassigned_count + 1
            call require(.not. query%found .and. .not. query%is_resolved .and. &
                query%has_reassignment .and. query%assignment_node_index == 0, &
                'same-scope reassignment was not a precise refusal')
        case ('nullified_callback')
            nullified_count = nullified_count + 1
            call require(.not. query%has_reassignment, &
                'NULLIFY was misclassified as reassignment')
        end select
    end do

    call require(reassigned_count == 1 .and. nullified_count == 1, &
        'reassignment and NULLIFY refusal cases were not both observed')
    print *, 'PASS: procedure-pointer reassignment refusal fact'

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

end program test_procedure_reassignment_refusal
