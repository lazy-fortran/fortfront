program test_select_type_generic_pass_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, subroutine_call_node, &
        control_statement_query_t, query_control_statement, &
        select_type_generic_dispatch_query_t, &
        query_select_type_generic_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(select_type_generic_dispatch_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, status, call_count

    call read_example('examples/f90/select_type_generic_pass_query.f90', source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o /tmp/fortfront_select_type_generic_pass_query '// &
        'examples/f90/select_type_generic_pass_query.f90', wait=.true., &
        exitstat=status)
    call require(status == 0, 'GNU Fortran rejected SELECT TYPE PASS fixture')
    call execute_command_line('/tmp/fortfront_select_type_generic_pass_query', &
        wait=.true., exitstat=status)
    call require(status == 0, 'SELECT TYPE PASS runtime oracle failed')
    call execute_command_line('rm -f /tmp/fortfront_select_type_generic_pass_query', &
        wait=.true.)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'SELECT TYPE PASS fixture was rejected: '// &
        trim(result%diagnostic_text))

    call_count = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'subroutine_call') cycle
        select type (node => result%arena%entries(i)%node)
            type is (subroutine_call_node)
            if (.not. allocated(node%name)) cycle
            if (index(trim(node%name), '%choose') <= 0) cycle
        end select
        call_count = call_count + 1
        query = query_select_type_generic_dispatch(result%arena, &
            arm_for_call(result, i), i)
        call require(query%found .and. query%is_resolved .and. &
            .not. query%is_refused .and. size(query%candidates) == 1 .and. &
            query%selected_candidate_index == 1 .and. &
            query%selected_procedure_node_index > 0 .and. &
            query%signature%found, 'SELECT TYPE generic target was not resolved')
        call require(query%candidates(1)%pass_metadata_resolved .and. &
            query%candidates(1)%pass_arg .and. &
            trim(query%candidates(1)%pass_name) == 'self' .and. &
            query%candidates(1)%pass_position == 2 .and. &
            query%candidates(1)%is_match, &
            'per-specific inherited PASS metadata was not exposed')
        call require(query%signature%dummy_count == 2 .and. &
            trim(query%signature%dummies(1)%name) == 'value' .and. &
            trim(query%signature%dummies(2)%name) == 'self', &
            'selected specific signature order was not preserved')
    end do

    call require(call_count == 1, 'unexpected SELECT TYPE generic call count')
    print *, 'PASS: SELECT TYPE generic PASS query contract'

contains

    include '../common/read_example.inc'

    integer function arm_for_call(frontend_result, call_index) result(arm_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        integer, intent(in) :: call_index
        type(control_statement_query_t) :: control
        integer :: i, j, current

        arm_index = 0
        do i = 1, frontend_result%arena%size
            if (.not. frontend_result%arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(frontend_result%arena, i)) /= &
                'select_type') cycle
            control = query_control_statement(frontend_result%arena, i)
            do j = 1, size(control%type_arms)
                current = call_index
                do while (current > 0 .and. &
                        frontend_result%arena%has_node_at(current))
                    if (current == control%type_arms(j)%arm_node_index) then
                        arm_index = current
                        return
                    end if
                    current = frontend_result%arena%entries(current)%parent_index
                end do
            end do
        end do
    end function arm_for_call

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_select_type_generic_pass_query
