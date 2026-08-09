program test_owned_array_class_is_dispatch
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, subroutine_call_node, &
        select_type_owned_array_dispatch_query_t, &
        query_select_type_owned_array_dispatch, STORAGE_OWNED
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_dispatch_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count, call_count, resolved_count, syntax_status
    logical :: saw_inherited, saw_override, saw_global, saw_alias, saw_control

    call read_example('examples/f90/owned_array_class_is_dispatch.f90', source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/owned_array_class_is_dispatch.f90', &
        wait=.true., exitstat=syntax_status)
    call require(syntax_status == 0, 'GNU Fortran rejected dispatch fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'dispatch fixture did not parse')

    select_count = 0
    call_count = 0
    resolved_count = 0
    saw_inherited = .false.
    saw_override = .false.
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
        if (index(trim(call_name(result, i)), '%run') <= 0) cycle
        call_count = call_count + 1
        query = query_select_type_owned_array_dispatch(result%arena, &
            arm_for_call(result, i), i)
        if (query%has_global_mutable_state) then
            saw_global = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%implementation_node_index == 0 .and. &
                index(query%refusal_reason, 'global') > 0, &
                'global owned-array dispatch was not refused')
        else if (query%has_unresolved_alias) then
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%implementation_node_index == 0, &
                'alias owned-array dispatch was not refused')
        else if (query%has_control_flow_boundary) then
            saw_control = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                query%implementation_node_index == 0, &
                'control-flow owned-array dispatch was not refused')
        else
            resolved_count = resolved_count + 1
            call require(query%found .and. query%is_owned_array .and. &
                query%is_resolved .and. .not. query%is_refused .and. &
                query%is_array_element_receiver .and. &
                .not. query%is_array_section_receiver .and. &
                query%receiver_declaration_index == query%selector_declaration_index .and. &
                query%receiver_storage%found .and. &
                query%receiver_storage%storage_class == STORAGE_OWNED .and. &
                query%receiver_storage%is_allocatable .and. &
                query%receiver_storage%is_polymorphic, &
                'owned-array receiver/storage mapping is wrong')
            call require(query%pass_arg .and. .not. query%is_nopass .and. &
                query%pass_metadata_resolved .and. &
                trim(query%pass_name) == 'self' .and. &
                query%pass_position == 2 .and. &
                query%implementation_pass_position == 2 .and. &
                query%signature%found .and. query%signature%dummy_count == 2 .and. &
                trim(query%signature%dummies(2)%name) == 'self', &
                'owned-array explicit PASS mapping is incomplete')
            if (trim(query%dynamic_type_name) == 'child_t') then
                saw_inherited = .true.
                call require(query%is_inherited .and. &
                    trim(query%implementation) == 'base_run', &
                    'inherited owned-array binding identity is wrong')
            else if (trim(query%dynamic_type_name) == 'override_t') then
                saw_override = .true.
                call require(.not. query%is_inherited .and. &
                    trim(query%implementation) == 'override_run', &
                    'direct owned-array binding identity is wrong')
            else
                call require(.false., 'unexpected owned-array dynamic type')
            end if
        end if
    end do

    call require(select_count == 5 .and. call_count == 5 .and. &
        resolved_count == 2 .and. saw_inherited .and. saw_override .and. &
        saw_global .and. saw_alias .and. saw_control, &
        'owned-array direct dispatch coverage is incomplete')
    print *, 'PASS: owned-array CLASS IS direct dispatch oracle'

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

end program test_owned_array_class_is_dispatch
