program test_owned_array_class_is_explicit_pass
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, subroutine_call_node, &
        select_type_owned_array_generic_dispatch_query_t, &
        query_select_type_owned_array_generic_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_generic_dispatch_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count, call_count, resolved_count, syntax_status
    logical :: saw_resolved, saw_global, saw_alias, saw_control

    call read_example('examples/f90/owned_array_class_is_explicit_pass.f90', &
        source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/owned_array_class_is_explicit_pass.f90', &
        wait=.true., exitstat=syntax_status)
    call require(syntax_status == 0, 'GNU Fortran rejected explicit PASS fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'explicit PASS fixture did not parse')

    select_count = 0
    call_count = 0
    resolved_count = 0
    saw_resolved = .false.
    saw_global = .false.
    saw_alias = .false.
    saw_control = .false.

    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'select_type') then
            select_count = select_count + 1
            control = query_control_statement(result%arena, i)
            call require(control%found .and. &
                control%statement_kind == CONTROL_SELECT_TYPE .and. &
                size(control%type_arms) == 1, &
                'SELECT TYPE arm facts are incomplete')
        end if
        if (trim(get_node_type_at(result%arena, i)) /= 'subroutine_call') cycle
        if (index(trim(call_name(result, i)), '%choose') <= 0) cycle
        call_count = call_count + 1
        query = query_select_type_owned_array_generic_dispatch(result%arena, &
            arm_for_call(result, i), i)
        if (query%has_global_mutable_state) then
            saw_global = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%selected_procedure_node_index == 0 .and. &
                index(query%refusal_reason, 'global') > 0, &
                'global explicit PASS dispatch was not refused')
        else if (query%has_unresolved_alias) then
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%selected_procedure_node_index == 0, &
                'alias explicit PASS dispatch was not refused')
        else if (query%has_control_flow_boundary) then
            saw_control = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%selected_procedure_node_index == 0, &
                'control-flow explicit PASS dispatch was not refused')
        else
            resolved_count = resolved_count + 1
            saw_resolved = .true.
            call require(query%found .and. query%is_owned_array .and. &
                query%is_resolved .and. .not. query%is_refused .and. &
                query%is_array_element_receiver .and. &
                query%receiver_declaration_index == query%selector_declaration_index .and. &
                query%receiver_storage%is_allocatable .and. &
                query%receiver_storage%is_polymorphic, &
                'explicit PASS receiver/storage mapping is wrong')
            call require(query%selected_pass_metadata_resolved .and. &
                query%selected_pass_arg .and. &
                .not. query%selected_is_nopass .and. &
                trim(query%selected_pass_name) == 'self' .and. &
                query%selected_pass_position == 2, &
                'selected explicit PASS mapping is incomplete')
            call require(size(query%candidates) == 1 .and. &
                query%candidates(1)%pass_metadata_resolved .and. &
                trim(query%candidates(1)%pass_name) == 'self' .and. &
                query%candidates(1)%pass_position == 2 .and. &
                query%candidates(1)%is_match, &
                'candidate explicit PASS mapping is incomplete')
        end if
    end do

    call require(select_count == 4 .and. call_count == 4 .and. &
        resolved_count == 1 .and. saw_resolved .and. saw_global .and. &
        saw_alias .and. saw_control, &
        'explicit PASS dispatch coverage is incomplete')
    print *, 'PASS: owned-array CLASS IS explicit PASS oracle'

contains

    include '../common/read_example.inc'

    function call_name(result, node_index) result(name)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: node_index
        character(len=:), allocatable :: name

        select type (node => result%arena%entries(node_index)%node)
            type is (subroutine_call_node)
                name = node%name
            class default
                name = ''
        end select
    end function call_name

    integer function arm_for_call(result, call_index) result(arm_index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: call_index
        type(control_statement_query_t) :: local_control
        integer :: i, j, current

        arm_index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
            local_control = query_control_statement(result%arena, i)
            do j = 1, size(local_control%type_arms)
                current = call_index
                do while (current > 0 .and. result%arena%has_node_at(current))
                    if (current == local_control%type_arms(j)%arm_node_index) then
                        arm_index = current
                        return
                    end if
                    current = result%arena%entries(current)%parent_index
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

end program test_owned_array_class_is_explicit_pass
