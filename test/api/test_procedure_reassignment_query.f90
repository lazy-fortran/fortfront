program test_procedure_reassignment_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_reassignment_call_query_t, &
        query_procedure_reassignment_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_reassignment_call_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, found_count

    call read_example('examples/f90/procedure_reassignment_call_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'procedure reassignment fixture was rejected')

    found_count = 0
    do i = 1, result%arena%size
        query = query_procedure_reassignment_call(result%arena, i)
        if (.not. query%found) cycle
        found_count = found_count + 1
        call require(query%call_node_index == i, &
            'reassignment call node identity was not preserved')
        call require(query%assignment_count == 2 .and. &
            query%has_reassignment, 'two-target reassignment facts are missing')
        call require(trim(query%pointer_name) == 'callback', &
            'procedure pointer name is wrong')
        call require(trim(query%first_target%procedure_name) == 'first_target', &
            'first reassignment target is wrong')
        call require(trim(query%second_target%procedure_name) == 'second_target', &
            'second reassignment target is wrong')
        call require(query%first_target%is_resolved .and. &
            query%second_target%is_resolved, &
            'reassignment targets were not resolved')
    end do

    call require(found_count == 1, &
        'bounded reassignment proof was not unique')
    query = query_procedure_reassignment_call(result%arena, -1)
    call require(.not. query%found .and. query%is_unresolved, &
        'invalid node received reassignment facts')
    print *, 'PASS: procedure-pointer reassignment query contract'

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

end program test_procedure_reassignment_query
