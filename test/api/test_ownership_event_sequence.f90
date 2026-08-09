program test_ownership_event_sequence
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, ownership_event_query_t, &
        query_ownership_events, OWNERSHIP_EVENT_ALLOCATE, &
        OWNERSHIP_EVENT_MOVE_ALLOC, OWNERSHIP_EVENT_ASSIGNMENT, &
        OWNERSHIP_EVENT_DEALLOCATE, OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE, &
        OWNERSHIP_REALLOCATION_POTENTIAL, OWNERSHIP_STATE_UNALLOCATED, &
        OWNERSHIP_STATE_ALLOCATED, OWNERSHIP_STATE_SAME_AS_SOURCE
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    character(:), allocatable :: source
    integer :: i, module_index

    call read_example('examples/f90/ownership_event_sequence_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected ownership sequence example')

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
    call require(size(events) == 5, 'ownership event sequence length is wrong')
    do i = 1, size(events)
        call require(events(i)%sequence_index == i, &
            'ownership event sequence index is not source ordered')
        select case (i)
        case (1, 2)
            call require(events(i)%event_kind == OWNERSHIP_EVENT_ALLOCATE .and. &
                events(i)%owner_state_before == OWNERSHIP_STATE_UNALLOCATED .and. &
                events(i)%owner_state_after == OWNERSHIP_STATE_ALLOCATED .and. &
                .not. events(i)%has_implicit_destination_deallocation .and. &
                .not. events(i)%has_potential_implicit_reallocation, &
                'ALLOCATE lifecycle contract is wrong')
        case (3)
            call require(events(i)%event_kind == OWNERSHIP_EVENT_MOVE_ALLOC .and. &
                events(i)%is_explicit_ownership_transfer .and. &
                events(i)%source_state_after == OWNERSHIP_STATE_UNALLOCATED .and. &
                events(i)%destination_state_after == &
                OWNERSHIP_STATE_SAME_AS_SOURCE .and. &
                events(i)%has_implicit_destination_deallocation, &
                'MOVE_ALLOC lifecycle contract is wrong')
        case (4)
            call require(events(i)%event_kind == OWNERSHIP_EVENT_ASSIGNMENT .and. &
                events(i)%assignment_kind == &
                OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE .and. &
                events(i)%reallocation_kind == OWNERSHIP_REALLOCATION_POTENTIAL .and. &
                events(i)%owner_state_after == OWNERSHIP_STATE_ALLOCATED .and. &
                events(i)%has_potential_implicit_reallocation .and. &
                .not. events(i)%is_explicit_ownership_transfer, &
                'allocatable assignment lifecycle contract is wrong')
        case (5)
            call require(events(i)%event_kind == OWNERSHIP_EVENT_DEALLOCATE .and. &
                events(i)%owner_state_before == OWNERSHIP_STATE_ALLOCATED .and. &
                events(i)%owner_state_after == OWNERSHIP_STATE_UNALLOCATED, &
                'DEALLOCATE lifecycle contract is wrong')
        end select
    end do

    print *, 'PASS: ownership event sequence API oracle'

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

end program test_ownership_event_sequence
