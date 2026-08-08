program test_ownership_reallocation_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, ownership_event_query_t, &
        query_ownership_events, OWNERSHIP_EVENT_ALLOCATE, &
        OWNERSHIP_EVENT_ASSIGNMENT, OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE, &
        OWNERSHIP_REALLOCATION_POTENTIAL, resolved_type_query_t, &
        query_resolved_type
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    character(:), allocatable :: source
    integer :: i, module_index, allocation_count, assignment_count
    type(resolved_type_query_t) :: lhs_type

    call read_example('examples/f90/ownership_reallocation_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected ownership reallocation example')

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
    allocation_count = 0
    assignment_count = 0
    do i = 1, size(events)
        select case (events(i)%event_kind)
        case (OWNERSHIP_EVENT_ALLOCATE)
            allocation_count = allocation_count + 1
            if (size(events(i)%shape_expr_indices) > 0) then
                call require(events(i)%rank == 1, &
                    'ALLOCATE rank fact is wrong')
                call require(events(i)%shape_expr_indices(1) > 0, &
                    'ALLOCATE shape expression fact is missing')
            end if
            if (events(i)%source_expr_index > 0) then
                call require(events(i)%mold_expr_index == 0, &
                    'ALLOCATE SOURCE= also reports MOLD=')
            end if
        case (OWNERSHIP_EVENT_ASSIGNMENT)
            assignment_count = assignment_count + 1
            call require(events(i)%assignment_kind == &
                OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE, &
                'whole allocatable assignment classification is wrong')
            call require(events(i)%reallocation_kind == &
                OWNERSHIP_REALLOCATION_POTENTIAL .and. &
                events(i)%is_potential_automatic_reallocation, &
                'automatic reallocation classification is wrong')
            call require(events(i)%lhs_owner_path%node_index > 0 .and. &
                events(i)%rhs_owner_path%node_index > 0, &
                'LHS/RHS owner paths are missing')
            call require(events(i)%lhs_rank == 1 .and. events(i)%rhs_rank == 1, &
                'LHS/RHS rank facts are wrong')
            lhs_type = query_resolved_type(result%arena, &
                events(i)%lhs_owner_path%node_index)
            call require(lhs_type%rank == events(i)%lhs_rank, &
                'LHS rank does not match the resolved type fact')
        end select
    end do

    call require(allocation_count == 2, 'ALLOCATE events are incomplete')
    call require(assignment_count == 1, &
        'indexed or ordinary assignments leaked into ownership events')
    print *, 'PASS: ownership reallocation facts API oracle'

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

end program test_ownership_reallocation_facts
