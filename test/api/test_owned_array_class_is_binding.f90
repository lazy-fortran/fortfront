program test_owned_array_class_is_binding
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_owned_array_binding_query_t, &
        query_select_type_owned_array_binding
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_binding_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count, syntax_status
    logical :: saw_direct, saw_inherited, saw_abstract
    logical :: saw_global, saw_alias, saw_control

    call read_example('examples/f90/owned_array_class_is_binding_identity.f90', &
        source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/owned_array_class_is_binding_identity.f90', &
        wait=.true., exitstat=syntax_status)
    call require(syntax_status == 0, 'GNU Fortran rejected binding fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'binding fixture did not parse')

    select_count = 0
    saw_direct = .false.
    saw_inherited = .false.
    saw_abstract = .false.
    saw_global = .false.
    saw_alias = .false.
    saw_control = .false.

    ! These expected values are an independent oracle for the fixture's
    ! storage, abstract-binding, and concrete-implementation contract.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        control = query_control_statement(result%arena, i)
        call require(control%found .and. &
            control%statement_kind == CONTROL_SELECT_TYPE .and. &
            size(control%type_arms) == 1, 'SELECT TYPE arm facts are incomplete')

        query = query_select_type_owned_array_binding(result%arena, &
            control%type_arms(1)%arm_node_index, 'run')
        if (query%has_global_mutable_state) then
            saw_global = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%implementation_node_index == 0 .and. &
                index(query%refusal_reason, 'global') > 0, &
                'global binding identity was not refused')
        else if (query%has_unresolved_alias) then
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%implementation_node_index == 0, &
                'alias binding identity was not refused')
        else if (query%has_control_flow_boundary) then
            saw_control = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%implementation_node_index == 0, &
                'control-flow binding identity was not refused')
        else if (trim(query%dynamic_type_name) == 'deferred_t') then
            saw_abstract = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. query%owned_array%is_declared_type_abstract, &
                'abstract guard binding identity was not refused')
        else
            call require(query%found .and. query%is_owned_array .and. &
                query%is_resolved .and. .not. query%is_refused .and. &
                query%is_declared_binding_deferred .and. &
                query%is_implementation_concrete, &
                'owned-array binding identity was not resolved')
            call require(trim(query%binding_name) == 'run' .and. &
                trim(query%declared_type_name) == 'base_t' .and. &
                trim(query%declared_binding%binding_name) == 'run' .and. &
                query%declared_binding%is_deferred .and. &
                query%declared_binding%binding_node_index > 0, &
                'abstract deferred binding facts are incomplete')
            call require(trim(query%implementation) == 'child_run' .and. &
                query%implementation_node_index > 0 .and. &
                trim(query%dynamic_binding%implementation) == 'child_run' .and. &
                query%dynamic_binding%implementation_node_index == &
                    query%implementation_node_index, &
                'concrete implementation target identity is wrong')
            if (trim(query%dynamic_type_name) == 'child_t') then
                saw_direct = .true.
                call require(.not. query%is_inherited, &
                    'direct child implementation was marked inherited')
            else if (trim(query%dynamic_type_name) == 'grandchild_t') then
                saw_inherited = .true.
                call require(query%is_inherited .and. &
                    trim(query%declaring_type_name) == 'child_t', &
                    'inherited implementation identity is wrong')
            else
                call require(.false., 'unexpected resolved dynamic type')
            end if
        end if
    end do

    call require(select_count == 6 .and. saw_direct .and. saw_inherited .and. &
        saw_abstract .and. saw_global .and. saw_alias .and. saw_control, &
        'owned-array binding identity coverage is incomplete')
    query = query_select_type_owned_array_binding(result%arena, 0, 'run')
    call require(.not. query%found .and. query%is_refused .and. &
        query%is_unresolved .and. query%implementation_node_index == 0, &
        'invalid owned-array binding query was not refused')
    print *, 'PASS: owned-array CLASS IS binding identity API oracle'

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

end program test_owned_array_class_is_binding
