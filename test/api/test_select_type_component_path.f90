program test_select_type_component_path
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, component_access_query_t, &
        query_component_access, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_component_query_t, query_select_type_component_path
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(component_access_query_t) :: access
    type(select_type_component_query_t) :: component
    character(len=:), allocatable :: source
    integer :: i, select_index, value_node, pointer_node
    integer :: child_arm, default_arm

    call read_example( &
        'examples/f90/select_type_component_path.f90', source)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'component-path fixture did not parse: '// &
        trim(result%error_msg))

    select_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'select_type') then
            select_index = i
            exit
        end if
    end do
    call require(select_index > 0, 'SELECT TYPE construct is absent')
    control = query_control_statement(result%arena, select_index)
    call require(control%found .and. control%statement_kind == CONTROL_SELECT_TYPE .and. &
        size(control%type_arms) == 2, 'SELECT TYPE arms are incomplete')
    child_arm = control%type_arms(1)%arm_node_index
    default_arm = control%type_arms(2)%arm_node_index

    value_node = 0
    pointer_node = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'component_access') cycle
        access = query_component_access(result%arena, i)
        if (trim(access%component_name) == 'value') value_node = i
        if (trim(access%component_name) == 'dynamic') pointer_node = i
    end do
    call require(value_node > 0 .and. pointer_node > 0, &
        'fixture component accesses are incomplete')

    component = query_select_type_component_path(result%arena, child_arm, value_node)
    call require(component%found .and. component%is_resolved .and. &
        component%is_selector_associate, 'narrowed alias path was not resolved')
    call require(size(component%component_path%component_names) == 2 .and. &
        trim(component%component_path%component_names(1)) == 'leaf' .and. &
        trim(component%component_path%component_names(2)) == 'value', &
        'nested component names are wrong')
    call require(size(component%component_path%component_declaration_indices) == 2 .and. &
        component%terminal_storage%found .and. &
        trim(component%terminal_storage%name) == 'value' .and. &
        component%terminal_storage%is_component, &
        'nested component declaration/storage facts are incomplete')

    component = query_select_type_component_path(result%arena, child_arm, pointer_node)
    call require(component%is_refused .and. component%is_unresolved .and. &
        .not. component%is_resolved, &
        'pointer component storage boundary was not refused')

    component = query_select_type_component_path(result%arena, default_arm, value_node)
    call require(component%is_refused .and. component%is_unresolved .and. &
        index(component%refusal_reason, 'CLASS DEFAULT') > 0, &
        'CLASS DEFAULT component narrowing was guessed')

    component = query_select_type_component_path(result%arena, 0, value_node)
    call require(component%is_refused .and. component%is_unresolved, &
        'invalid SELECT TYPE arm was not refused')
    print *, 'PASS: SELECT TYPE component path contract'

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

end program test_select_type_component_path
