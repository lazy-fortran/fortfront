program test_owned_array_class_is_generic_dispatch
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        subroutine_call_node, &
        select_type_owned_array_generic_dispatch_query_t, &
        query_select_type_owned_array_generic_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_generic_dispatch_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count, call_count, resolved_count, syntax_status
    logical :: saw_integer, saw_real, saw_global, saw_alias, saw_control

    call read_example('examples/f90/owned_array_class_is_generic_dispatch.f90', &
        source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/owned_array_class_is_generic_dispatch.f90', &
        wait=.true., exitstat=syntax_status)
    call require(syntax_status == 0, 'GNU Fortran rejected generic fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'generic fixture did not parse')

    select_count = 0
    call_count = 0
    resolved_count = 0
    saw_integer = .false.
    saw_real = .false.
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
                .not. query%is_resolved .and. query%selected_procedure_node_index == 0 .and. &
                index(query%refusal_reason, 'global') > 0, &
                'global generic dispatch was not refused')
        else if (query%has_unresolved_alias) then
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%selected_procedure_node_index == 0, &
                'alias generic dispatch was not refused')
        else if (query%has_control_flow_boundary) then
            saw_control = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%selected_procedure_node_index == 0, &
                'control-flow generic dispatch was not refused')
        else
            resolved_count = resolved_count + 1
            call require(query%found .and. query%is_owned_array .and. &
                query%is_resolved .and. .not. query%is_refused .and. &
                query%is_generic_binding .and. query%is_array_element_receiver .and. &
                .not. query%is_array_section_receiver, &
                'owned-array generic dispatch was not resolved')
            call require(trim(query%receiver_name) == 'values(1)' .and. &
                query%receiver_node_index == 0 .and. &
                query%receiver_declaration_index == query%selector_declaration_index .and. &
                query%receiver_storage%is_allocatable .and. &
                query%receiver_storage%is_polymorphic, &
                'owned-array receiver storage mapping is wrong')
            call require(query%pass_arg .and. query%pass_position == 1 .and. &
                len_trim(query%pass_name) == 0 .and. query%signature%found .and. &
                query%signature%dummy_count == 2 .and. &
                trim(query%signature%dummies(1)%name) == 'self', &
                'owned-array PASS facts are incomplete')
            call require(size(query%candidates) == 2 .and. &
                count_matches(query) == 1 .and. &
                query%selected_candidate_index > 0 .and. &
                query%selected_procedure_node_index > 0, &
                'owned-array generic candidate facts are incomplete')
            if (trim(query%candidates(query%selected_candidate_index)%procedure_name) == &
                    'choose_int') then
                saw_integer = .true.
            else if (trim(query%candidates(query%selected_candidate_index)%procedure_name) == &
                    'choose_real') then
                saw_real = .true.
            else
                call require(.false., 'wrong owned-array generic specific')
            end if
        end if
    end do

    call require(select_count == 5 .and. call_count == 5 .and. &
        resolved_count == 2 .and. saw_integer .and. saw_real .and. &
        saw_global .and. saw_alias .and. saw_control, &
        'owned-array generic dispatch coverage is incomplete')
    print *, 'PASS: owned-array CLASS IS generic/PASS dispatch oracle'

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

    integer function count_matches(query) result(count)
        type(select_type_owned_array_generic_dispatch_query_t), intent(in) :: query
        integer :: i

        count = 0
        do i = 1, size(query%candidates)
            if (query%candidates(i)%is_match) count = count + 1
        end do
    end function count_matches

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_owned_array_class_is_generic_dispatch
