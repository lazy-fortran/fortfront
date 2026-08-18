program test_select_type_real8_expression_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, &
        control_statement_query_t, query_control_statement, &
        select_type_generic_dispatch_query_t, &
        query_select_type_generic_dispatch, resolved_type_query_t, &
        query_resolved_type, TREAL
    use ast_nodes_core, only: call_or_subscript_node, binary_op_node
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(select_type_generic_dispatch_query_t) :: dispatch
    type(control_statement_query_t) :: control
    type(resolved_type_query_t) :: resolved
    character(len=:), allocatable :: source, executable
    integer :: i, arm_index, call_index
    logical :: found_call, found_binary

    call read_example( &
        'examples/f90/select_type_real8_expression_query.f90', source)
    executable = test_executable_path('fortfront_select_type_real8_expression_query')
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/select_type_real8_expression_query.f90', wait=.true., &
        exitstat=i)
    call require(i == 0, 'GNU Fortran rejected REAL(8) expression fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=i)
    call require(i == 0, 'REAL(8) expression runtime oracle failed')
    call test_remove_file(executable)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'REAL(8) expression fixture was rejected: '// &
        trim(result%diagnostic_text))

    found_call = .false.
    found_binary = .false.
    call_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'call_or_subscript') then
            select type (node => result%arena%entries(i)%node)
                type is (call_or_subscript_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) /= 'apply') cycle
                found_call = .true.
                call_index = i
                call require(allocated(node%arg_indices) .and. &
                    size(node%arg_indices) == 1, &
                    'REAL(8) generic call argument count is wrong')
            end select
        else if (trim(get_node_type_at(result%arena, i)) == 'binary_op') then
            select type (node => result%arena%entries(i)%node)
                type is (binary_op_node)
                if (.not. allocated(node%operator)) cycle
                if (trim(node%operator) /= '+') cycle
                resolved = query_resolved_type(result%arena, i)
                if (.not. resolved%found) cycle
                if (resolved%type_kind /= TREAL .or. &
                    resolved%kind_value /= 8 .or. resolved%rank /= 0) cycle
                found_binary = .true.
            end select
        end if
    end do
    call require(found_call, 'SELECT TYPE REAL(8) generic call was not found')
    call require(found_binary, 'scalar REAL(8) expression type was not resolved')

    arm_index = arm_for_call(result, call_index)
    call require(arm_index > 0, 'SELECT TYPE arm for REAL(8) call was not found')
    dispatch = query_select_type_generic_dispatch(result%arena, arm_index, &
        call_index)
    call require(dispatch%found .and. dispatch%is_resolved .and. &
        .not. dispatch%is_refused .and. dispatch%selected_candidate_index == 1, &
        'REAL(8) SELECT TYPE generic specific was not resolved')
    call require(dispatch%signature%result_type_known .and. &
        dispatch%signature%result_kind_known .and. &
        dispatch%signature%result_type_kind == TREAL .and. &
        dispatch%signature%result_kind_value == 8 .and. &
        dispatch%signature%result_rank_known .and. &
        dispatch%signature%result_rank == 0, &
        'REAL(8) specific result facts are incomplete')
    call require(dispatch%signature%dummy_count == 2, &
        'REAL(8) specific dummy count is wrong')
    call require(dispatch%signature%dummies(2)%type_known .and. &
        dispatch%signature%dummies(2)%kind_known .and. &
        dispatch%signature%dummies(2)%type_kind == TREAL .and. &
        dispatch%signature%dummies(2)%kind_value == 8 .and. &
        dispatch%signature%dummies(2)%rank_known .and. &
        dispatch%signature%dummies(2)%rank == 0, &
        'REAL(8) specific dummy facts are incomplete')
    call require(dispatch%candidates(1)%is_match, &
        'REAL(8) expression did not match the specific signature')

    print *, 'PASS: SELECT TYPE REAL(8) expression query contract'

contains

    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'

    integer function arm_for_call(frontend_result, target_index) result(arm_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        integer, intent(in) :: target_index
        integer :: i, j, current

        arm_index = 0
        do i = 1, frontend_result%arena%size
            if (.not. frontend_result%arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(frontend_result%arena, i)) /= &
                'select_type') cycle
            control = query_control_statement(frontend_result%arena, i)
            do j = 1, size(control%type_arms)
                current = target_index
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

end program test_select_type_real8_expression_query
