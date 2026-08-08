program test_ownership_lifetime_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, storage_query_t, query_storage, &
        STORAGE_OWNED, ownership_event_query_t, query_ownership_events, &
        OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE, &
        OWNERSHIP_EVENT_MOVE_ALLOC, OWNERSHIP_EVENT_ASSIGNMENT
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    type(storage_query_t) :: storage
    character(:), allocatable :: source
    integer :: i, module_index, allocate_count, deallocate_count
    integer :: move_count, assignment_count, source_allocate_count
    integer :: owner_component_count

    call read_example('examples/f90/ownership_lifetime_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected ownership lifetime example')

    module_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'module_node') then
            module_index = i
            exit
        end if
    end do
    call require(module_index > 0, 'module node missing')

    events = query_ownership_events(result%arena, module_index)
    allocate_count = 0
    deallocate_count = 0
    move_count = 0
    assignment_count = 0
    source_allocate_count = 0
    owner_component_count = 0
    do i = 1, size(events)
        select case (events(i)%event_kind)
        case (OWNERSHIP_EVENT_ALLOCATE)
            allocate_count = allocate_count + 1
            call require(events(i)%owner_path%node_index > 0 .and. &
                events(i)%owner_path%base_node_index > 0, &
                'ALLOCATE owner path is missing')
            if (events(i)%owner_path%found) then
                owner_component_count = owner_component_count + 1
                call require(size(events(i)%owner_path%component_names) == 1, &
                    'ALLOCATE owner component path depth is wrong')
                call require(trim(events(i)%owner_path%component_names(1)) == &
                    'owner', 'ALLOCATE owner component path is wrong')
                storage = query_storage(result%arena, &
                    events(i)%owner_path%node_index)
                call require(storage%found .and. storage%is_allocatable .and. &
                    storage%storage_class == STORAGE_OWNED, &
                    'ALLOCATE owner storage fact is wrong')
            end if
            call require(.not. events(i)%is_potential_automatic_reallocation .and. &
                .not. events(i)%is_explicit_ownership_transfer, &
                'ALLOCATE lifetime flags are wrong')
            if (events(i)%source_expr_index > 0) then
                source_allocate_count = source_allocate_count + 1
                call require(events(i)%source_path%base_node_index > 0 .and. &
                    size(events(i)%source_path%component_names) == 0, &
                    'ALLOCATE SOURCE path is wrong')
            end if
        case (OWNERSHIP_EVENT_DEALLOCATE)
            deallocate_count = deallocate_count + 1
            call require(events(i)%owner_path%node_index > 0 .and. &
                events(i)%owner_path%base_node_index > 0, &
                'DEALLOCATE owner path is missing')
            call require(.not. events(i)%is_potential_automatic_reallocation .and. &
                .not. events(i)%is_explicit_ownership_transfer, &
                'DEALLOCATE lifetime flags are wrong')
        case (OWNERSHIP_EVENT_MOVE_ALLOC)
            move_count = move_count + 1
            call require(events(i)%source_path%base_node_index > 0 .and. &
                events(i)%destination_path%found, &
                'MOVE_ALLOC source/destination paths are missing')
            call require(size(events(i)%destination_path%component_names) == 1, &
                'MOVE_ALLOC destination path depth is wrong')
            call require(trim(events(i)%destination_path%component_names(1)) == &
                'destination', 'MOVE_ALLOC destination path is wrong')
            call require(.not. events(i)%is_potential_automatic_reallocation .and. &
                events(i)%is_explicit_ownership_transfer, &
                'MOVE_ALLOC lifetime flags are wrong')
        case (OWNERSHIP_EVENT_ASSIGNMENT)
            assignment_count = assignment_count + 1
            call require(events(i)%is_potential_automatic_reallocation .and. &
                .not. events(i)%is_explicit_ownership_transfer, &
                'automatic reallocation flag is wrong')
            call require(events(i)%owner_path%found .and. &
                events(i)%destination_path%found .and. &
                events(i)%source_path%base_node_index > 0, &
                'assignment lifetime paths are missing')
        end select
    end do

    call require(allocate_count == 2 .and. source_allocate_count == 1, &
        'ALLOCATE ownership events are incomplete')
    call require(owner_component_count == 1, &
        'ALLOCATE component owner path is incomplete')
    call require(deallocate_count == 1 .and. move_count == 1, &
        'explicit lifetime events are incomplete')
    call require(assignment_count == 1, &
        'automatic reallocation event is missing')

    print *, 'PASS: ownership lifetime facts API oracle'

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

end program test_ownership_lifetime_facts
