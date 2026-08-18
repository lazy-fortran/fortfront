program test_select_type_component_binding
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, component_access_query_t, &
        query_component_access, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_component_binding_query_t, &
        query_select_type_component_binding
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(component_access_query_t) :: access
    type(select_type_component_binding_query_t) :: binding
    character(len=:), allocatable :: source
    integer :: i, select_index, arm, leaf_node, generic_node
    integer :: pointer_node, allocatable_node

    call read_example( &
        'examples/f90/select_type_component_binding.f90', source)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'component-binding fixture did not parse: '// &
        trim(result%error_msg))

    select_index = 0
    leaf_node = 0
    generic_node = 0
    pointer_node = 0
    allocatable_node = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'select_type') then
            select_index = i
        else if (trim(get_node_type_at(result%arena, i)) == 'component_access') then
            access = query_component_access(result%arena, i)
            if (.not. access%found) cycle
            select case (trim(access%component_name))
            case ('leaf')
                leaf_node = i
            case ('generic')
                generic_node = i
            case ('dynamic')
                pointer_node = i
            case ('owned')
                allocatable_node = i
            end select
        end if
    end do
    call require(select_index > 0 .and. leaf_node > 0 .and. &
        generic_node > 0 .and. pointer_node > 0 .and. allocatable_node > 0, &
        'fixture SELECT TYPE component paths are incomplete')
    control = query_control_statement(result%arena, select_index)
    call require(control%found .and. control%statement_kind == CONTROL_SELECT_TYPE .and. &
        size(control%type_arms) == 1, 'fixture SELECT TYPE arm is incomplete')
    arm = control%type_arms(1)%arm_node_index

    ! Independent oracle: leaf_t inherits the concrete implementation from
    ! impl_t, while no generic or dynamic/owned storage target is static.
    binding = query_select_type_component_binding(result%arena, arm, leaf_node, 'run')
    call require(binding%found .and. binding%is_resolved .and. &
        binding%is_inherited .and. trim(binding%component_type_name) == 'leaf_t' .and. &
        trim(binding%declaring_type_name) == 'impl_t' .and. &
        trim(binding%implementation) == 'impl_run', &
        'inherited component binding was not resolved')
    call require(binding%hierarchy%implementation_node_index > 0 .and. &
        .not. binding%is_refused, 'resolved component binding retained refusal')

    binding = query_select_type_component_binding(result%arena, arm, generic_node, &
        'choose')
    call require(binding%is_refused .and. binding%is_unresolved .and. &
        binding%is_generic, 'generic component binding was guessed')

    binding = query_select_type_component_binding(result%arena, arm, pointer_node, &
        'run')
    call require(binding%is_refused .and. binding%is_unresolved .and. &
        binding%is_pointer_boundary, 'pointer component boundary was guessed')

    binding = query_select_type_component_binding(result%arena, arm, allocatable_node, &
        'run')
    call require(binding%is_refused .and. binding%is_unresolved .and. &
        binding%is_allocatable_boundary, &
        'allocatable component boundary was guessed')

    binding = query_select_type_component_binding(result%arena, arm, leaf_node, &
        'missing')
    call require(binding%is_refused .and. binding%is_unresolved .and. &
        index(binding%refusal_reason, 'unresolved') > 0, &
        'unresolved component binding was guessed')

    print *, 'PASS: SELECT TYPE component binding contract'

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

end program test_select_type_component_binding
