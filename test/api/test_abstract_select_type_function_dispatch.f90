program test_abstract_select_type_function_dispatch
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, assignment_node, &
        derived_type_query_t, query_derived_type, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_branch_query_t, query_select_type_branch, &
        select_type_dispatch_query_t, query_select_type_dispatch, &
        type_bound_call_query_t, query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_branch_query_t) :: branch
    type(select_type_dispatch_query_t) :: dispatch
    type(type_bound_call_query_t) :: call_facts
    character(len=:), allocatable :: source, syntax_command
    integer :: syntax_status, syntax_exitstat
    integer :: i, j, body_index, call_index
    logical :: saw_leaf, saw_default, saw_global

    call read_example( &
        'examples/f90/abstract_select_type_function_dispatch.f90', source)
    syntax_command = 'gfortran -fsyntax-only examples/f90/'// &
        'abstract_select_type_function_dispatch.f90'
    call execute_command_line(syntax_command, wait=.true., &
        exitstat=syntax_exitstat, cmdstat=syntax_status)
    call require(syntax_status == 0 .and. syntax_exitstat == 0, &
        'GNU Fortran rejected the abstract function-dispatch fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'abstract function-dispatch fixture did not parse')

    saw_leaf = .false.
    saw_default = .false.
    saw_global = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        control = query_control_statement(result%arena, i)
        if (.not. control%found) cycle
        do j = 1, size(control%type_arms)
            branch = query_select_type_branch(result%arena, &
                control%type_arms(j)%arm_node_index)
            if (.not. allocated(control%type_arms(j)%body_node_indices)) cycle
            if (size(control%type_arms(j)%body_node_indices) /= 1) cycle
            body_index = control%type_arms(j)%body_node_indices(1)
            call_index = body_index
            if (result%arena%has_node_at(body_index)) then
                select type (body => result%arena%entries(body_index)%node)
                    type is (assignment_node)
                    call_index = body%value_index
                end select
            end if
            call_facts = query_type_bound_call(result%arena, call_index)
            if (trim(call_facts%binding_name) /= 'evaluate') cycle
            dispatch = query_select_type_dispatch(result%arena, &
                control%type_arms(j)%arm_node_index, call_index)
            if (trim(branch%selector_name) == 'global_object') then
                call require(dispatch%is_refused .and. &
                    dispatch%is_ownership_changing .and. &
                    dispatch%implementation_node_index == 0, &
                    'global mutable selector crossed the dispatch boundary')
                saw_global = .true.
            else if (trim(branch%guard_type_name) == 'leaf_t') then
                call require(dispatch%found .and. dispatch%is_resolved .and. &
                    .not. dispatch%is_refused .and. .not. dispatch%is_unresolved, &
                    'concrete function dispatch was refused')
                call require(dispatch%is_function_reference .and. &
                    trim(dispatch%receiver_name) == 'object' .and. &
                    trim(dispatch%implementation) == 'leaf_evaluate', &
                    'function reference or concrete implementation facts are wrong')
                call require(trim(dispatch%concrete_type_name) == 'leaf_t' .and. &
                    dispatch%implementation_node_index > 0 .and. &
                    dispatch%dispatch_boundary_known .and. &
                    dispatch%arm_entry_node_index == body_index .and. &
                    dispatch%arm_exit_node_index == body_index, &
                    'function dispatch assignment boundary was not preserved')
                saw_leaf = .true.
            else if (branch%is_class_default) then
                call require(dispatch%is_refused .and. &
                    .not. dispatch%is_resolved .and. dispatch%is_unresolved .and. &
                    len_trim(dispatch%refusal_reason) > 0, &
                    'CLASS DEFAULT function dispatch was not refused')
                saw_default = .true.
            end if
        end do
    end do

    call require(saw_leaf .and. saw_default .and. saw_global, &
        'expected concrete and refusal cases were not enumerated')
    print *, 'PASS: abstract SELECT TYPE function dispatch contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_abstract_select_type_function_dispatch
