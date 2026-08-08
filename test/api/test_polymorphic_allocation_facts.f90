program test_polymorphic_allocation_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, declaration_query_t, &
        query_declaration, ownership_event_query_t, query_ownership_events, &
        OWNERSHIP_EVENT_ALLOCATE, polymorphic_allocation_query_t, &
        query_polymorphic_allocation, POLYMORPHIC_SOURCE_CONCRETE, &
        POLYMORPHIC_SOURCE_POLYMORPHIC
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    type(polymorphic_allocation_query_t) :: fact, direct_fact
    type(declaration_query_t) :: declaration
    character(:), allocatable :: source
    integer :: i, module_index, allocate_count
    integer :: owner_declaration, universal_declaration
    integer :: repeated_declaration, factory_declaration, alias_declaration
    integer :: repeated_count, polymorphic_count
    integer :: alias_count, factory_count, first_allocation_node
    logical :: found_component

    call read_example('examples/f90/polymorphic_allocation_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected polymorphic allocation example')

    module_index = 0
    owner_declaration = 0
    universal_declaration = 0
    repeated_declaration = 0
    factory_declaration = 0
    alias_declaration = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        select case (trim(get_node_type_at(result%arena, i)))
        case ('module_node')
            module_index = i
        case ('declaration')
            declaration = query_declaration(result%arena, i)
            if (.not. declaration%found) cycle
            select case (trim(declaration%name))
            case ('owner')
                owner_declaration = i
            case ('universal')
                universal_declaration = i
            case ('repeated')
                repeated_declaration = i
            case ('factory_owner')
                factory_declaration = i
            case ('alias_source_owner')
                alias_declaration = i
            end select
        end select
    end do
    call require(module_index > 0, 'module node is missing')
    call require(owner_declaration > 0 .and. universal_declaration > 0 .and. &
        repeated_declaration > 0 .and. factory_declaration > 0 .and. &
        alias_declaration > 0, 'polymorphic allocation declarations are missing')

    events = query_ownership_events(result%arena, module_index)
    allocate_count = 0
    repeated_count = 0
    polymorphic_count = 0
    alias_count = 0
    factory_count = 0
    first_allocation_node = 0
    found_component = .false.
    do i = 1, size(events)
        if (events(i)%event_kind /= OWNERSHIP_EVENT_ALLOCATE) cycle
        fact = events(i)%polymorphic_allocation
        if (.not. fact%found) cycle
        allocate_count = allocate_count + 1
        if (first_allocation_node == 0) first_allocation_node = events(i)%node_index
        call require(fact%allocation_node_index == events(i)%node_index, &
            'allocation fact lost its event identity')
        call require(fact%owner_node_index > 0 .and. fact%source_expr_index > 0, &
            'allocation/source expression indices are missing')
        call require(fact%owner_path%node_index == fact%owner_node_index, &
            'owner node/path identity is inconsistent')

        if (fact%owner_declaration_index == owner_declaration) then
            call require(fact%is_bounded .and. fact%is_source_concrete .and. &
                .not. fact%is_source_polymorphic .and. &
                .not. fact%is_source_unknown, 'class(base_t) concrete fact is wrong')
            call require(trim(fact%owner_declared_type) == 'class(base_t)', &
                'class(base_t) owner declaration was not exposed')
            call require(trim(fact%source_resolved_type) == 'child_t', &
                'concrete source type was not resolved')
            call require(fact%source_classification == POLYMORPHIC_SOURCE_CONCRETE, &
                'concrete source classification is wrong')
        else if (fact%owner_declaration_index == universal_declaration) then
            call require(fact%is_bounded .and. &
                trim(fact%owner_declared_type) == 'class(*)', &
                'class(*) concrete fact is wrong')
        else if (fact%owner_declaration_index == repeated_declaration) then
            repeated_count = repeated_count + 1
            call require(.not. fact%is_bounded .and. fact%is_repeated_acquisition .and. &
                fact%is_source_concrete, 'repeated acquisition was accepted')
        else if (fact%owner_declaration_index == factory_declaration) then
            factory_count = factory_count + 1
            call require(.not. fact%is_bounded .and. fact%is_factory_source .and. &
                fact%is_source_unknown, 'factory source was guessed as concrete')
        else if (fact%owner_declaration_index == alias_declaration) then
            if (fact%is_source_polymorphic) then
                polymorphic_count = polymorphic_count + 1
                call require(fact%source_classification == &
                    POLYMORPHIC_SOURCE_POLYMORPHIC .and. &
                    fact%is_source_polymorphic .and. .not. fact%is_bounded, &
                    'polymorphic source was not retained as a refusal')
            else
                alias_count = alias_count + 1
                call require(.not. fact%is_bounded .and. fact%is_alias .and. &
                    fact%is_source_unknown, 'alias source was guessed as concrete')
            end if
        else if (is_payload_path(fact)) then
            found_component = .true.
            call require(fact%is_bounded .and. fact%owner_path%found .and. &
                fact%source_path%found, 'allocatable component path fact is incomplete')
            call require_source_component_path(fact)
        else
            polymorphic_count = polymorphic_count + 1
            call require(fact%source_classification == &
                POLYMORPHIC_SOURCE_POLYMORPHIC .and. &
                fact%is_source_polymorphic .and. .not. fact%is_bounded, &
                'polymorphic source was not retained as a refusal')
        end if
    end do

    call require(allocate_count == 8, 'polymorphic allocation event count is wrong')
    call require(repeated_count == 2, 'repeated acquisition facts are incomplete')
    call require(factory_count == 1 .and. alias_count == 1, &
        'factory/alias refusal facts are incomplete')
    call require(polymorphic_count == 1, 'dynamic source refusal fact is incomplete')
    call require(found_component, 'allocatable component source fact is missing')

    call require(first_allocation_node > 0, 'allocation event oracle found no allocation')
    direct_fact = query_polymorphic_allocation(result%arena, first_allocation_node)
    call require(direct_fact%allocation_node_index == first_allocation_node, &
        'direct allocation query did not return its node identity')

    print *, 'PASS: polymorphic allocation/source facts API oracle'

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

    logical function is_payload_path(fact)
        type(polymorphic_allocation_query_t), intent(in) :: fact

        is_payload_path = .false.
        if (size(fact%owner_path%component_names) /= 1) return
        is_payload_path = trim(fact%owner_path%component_names(1)) == 'payload'
    end function is_payload_path

    subroutine require_source_component_path(fact)
        type(polymorphic_allocation_query_t), intent(in) :: fact

        call require(size(fact%source_path%component_names) == 1, &
            'component SOURCE= path has the wrong length')
        call require(trim(fact%source_path%component_names(1)) == 'concrete', &
            'component SOURCE= path is incomplete')
    end subroutine require_source_component_path

end program test_polymorphic_allocation_facts
