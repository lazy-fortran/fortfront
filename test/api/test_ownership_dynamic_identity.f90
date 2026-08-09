program test_ownership_dynamic_identity
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, declaration_query_t, &
        query_declaration, ownership_event_query_t, query_ownership_events, &
        OWNERSHIP_EVENT_MOVE_ALLOC, OWNERSHIP_EVENT_ASSIGNMENT, &
        OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE, &
        STORAGE_MODULE
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    type(declaration_query_t) :: declaration
    character(:), allocatable :: source
    integer :: i, module_index, status
    integer :: seed_index, source_index, destination_index
    integer :: replacement_index, aliased_index, shared_index
    integer :: move_count, assignment_count

    call read_example('examples/f90/ownership_dynamic_identity_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected dynamic identity fixture')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/ownership_dynamic_identity_facts.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the dynamic identity fixture')

    module_index = 0
    seed_index = 0
    source_index = 0
    destination_index = 0
    replacement_index = 0
    aliased_index = 0
    shared_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        select case (trim(get_node_type_at(result%arena, i)))
        case ('module_node')
            module_index = i
        case ('declaration')
            declaration = query_declaration(result%arena, i)
            if (.not. declaration%found) cycle
            select case (trim(declaration%name))
            case ('seed')
                seed_index = i
            case ('source')
                source_index = i
            case ('destination')
                destination_index = i
            case ('replacement')
                replacement_index = i
            case ('aliased')
                aliased_index = i
            case ('shared')
                shared_index = i
            end select
        end select
    end do
    call require(module_index > 0 .and. seed_index > 0 .and. source_index > 0 .and. &
        destination_index > 0 .and. replacement_index > 0 .and. &
        aliased_index > 0 .and. shared_index > 0, &
        'dynamic identity declarations are incomplete')

    events = query_ownership_events(result%arena, module_index)
    move_count = 0
    assignment_count = 0
    do i = 1, size(events)
        select case (events(i)%event_kind)
        case (OWNERSHIP_EVENT_MOVE_ALLOC)
            move_count = move_count + 1
            call require(events(i)%lhs_rank == 1 .and. events(i)%rhs_rank == 1, &
                'array MOVE_ALLOC rank facts are missing')
            if (events(i)%source_declaration_index == seed_index) then
                call require(events(i)%destination_declaration_index == source_index .and. &
                    events(i)%is_source_dynamic_type_known .and. &
                    trim(events(i)%source_dynamic_type) == 'child_t' .and. &
                    events(i)%is_destination_dynamic_type_known .and. &
                    trim(events(i)%destination_dynamic_type) == 'child_t', &
                    'concrete-to-polymorphic MOVE_ALLOC identity is wrong')
            else if (events(i)%source_declaration_index == source_index) then
                call require(events(i)%destination_declaration_index == destination_index .and. &
                    events(i)%is_source_dynamic_type_known .and. &
                    events(i)%is_destination_dynamic_type_known .and. &
                    trim(events(i)%destination_dynamic_type) == 'child_t', &
                    'flow-sensitive MOVE_ALLOC identity is wrong')
            else if (events(i)%source_declaration_index == aliased_index) then
                call require(events(i)%is_refused .and. &
                    events(i)%has_unresolved_alias .and. &
                    events(i)%has_dynamic_type_boundary .and. &
                    .not. events(i)%is_destination_dynamic_type_known, &
                    'TARGET MOVE_ALLOC was not refused')
            else if (events(i)%source_declaration_index == shared_index) then
                call require(events(i)%is_refused .and. &
                    events(i)%has_global_mutable_state .and. &
                    events(i)%has_dynamic_type_boundary .and. &
                    events(i)%source_storage_class == STORAGE_MODULE, &
                    'module-state MOVE_ALLOC was not refused')
            else
                call require(.false., 'unexpected MOVE_ALLOC source')
            end if
        case (OWNERSHIP_EVENT_ASSIGNMENT)
            assignment_count = assignment_count + 1
            call require(events(i)%source_declaration_index == replacement_index .and. &
                events(i)%destination_declaration_index == destination_index .and. &
                events(i)%is_source_dynamic_type_known .and. &
                events(i)%is_destination_dynamic_type_known .and. &
                trim(events(i)%destination_dynamic_type) == 'child_t', &
                'polymorphic reallocation identity is wrong')
        case (OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE)
        end select
    end do
    call require(move_count == 4 .and. assignment_count == 1, &
        'dynamic identity event coverage is incomplete')
    print *, 'PASS: ownership dynamic identity API oracle'

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

end program test_ownership_dynamic_identity
