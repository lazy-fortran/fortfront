program test_polymorphic_assignment_replay
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, ownership_event_query_t, &
        query_ownership_events, polymorphic_assignment_query_t, &
        query_polymorphic_assignment_into, OWNERSHIP_EVENT_ASSIGNMENT, &
        OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE, &
        OWNERSHIP_REALLOCATION_POTENTIAL
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    type(polymorphic_assignment_query_t) :: fact, direct_fact
    character(:), allocatable :: source
    integer :: i, program_index, assignment_count, status
    integer :: replayable_count, polymorphic_source_count, alias_count
    integer :: global_count, control_count
    logical :: found_replayable_assignment
    character(len=*), parameter :: fixture = &
        'examples/f90/polymorphic_assignment_replay_facts.f90'
    character(len=:), allocatable :: executable

    call read_example(fixture, source)
    executable = test_executable_path('fortfront_polymorphic_assignment_replay_oracle')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected polymorphic assignment fixture')

    program_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'program') then
            program_index = i
            exit
        end if
    end do
    call require(program_index > 0, 'program node is missing')

    events = query_ownership_events(result%arena, program_index)
    assignment_count = 0
    replayable_count = 0
    polymorphic_source_count = 0
    alias_count = 0
    global_count = 0
    control_count = 0
    found_replayable_assignment = .false.
    do i = 1, size(events)
        if (events(i)%event_kind /= OWNERSHIP_EVENT_ASSIGNMENT) cycle
        assignment_count = assignment_count + 1
        fact = events(i)%polymorphic_assignment
        if (.not. fact%found) cycle
        if (fact%is_replayable) then
            replayable_count = replayable_count + 1
            found_replayable_assignment = .true.
            call require(fact%is_dynamic_type_known .and. &
                fact%is_source_concrete .and. &
                .not. fact%is_source_polymorphic .and. &
                fact%is_destination_polymorphic, &
                'polymorphic assignment type facts are incomplete')
            call require(fact%has_owned_components, &
                'allocatable derived component ownership was not retained')
            call require(trim(fact%dynamic_type) == 'child_t', &
                'source dynamic type does not match the semantic oracle')
            call require(fact%destination_path%found, &
                'polymorphic component destination path is missing')
            call require(size(fact%destination_path%component_names) == 1, &
                'polymorphic component destination path has wrong depth')
            call require(trim(fact%destination_path%component_names(1)) == &
                'item', 'polymorphic component destination path is incomplete')
            call require(fact%destination_declaration_index > 0 .and. &
                fact%source_declaration_index > 0, &
                'polymorphic assignment storage identities are missing')
            call require(events(i)%assignment_kind == &
                OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE .and. &
                events(i)%reallocation_kind == OWNERSHIP_REALLOCATION_POTENTIAL .and. &
                events(i)%is_destination_dynamic_type_known .and. &
                trim(events(i)%destination_dynamic_type) == 'child_t' .and. &
                .not. events(i)%has_dynamic_type_boundary, &
                'ownership event did not carry replay dynamic-type facts')
            call query_polymorphic_assignment_into(result%arena, events(i)%node_index, &
                direct_fact)
            call require(direct_fact%is_replayable .and. &
                direct_fact%assignment_node_index == events(i)%node_index, &
                'direct polymorphic assignment query disagrees with event query')
        else if (fact%is_source_polymorphic) then
            polymorphic_source_count = polymorphic_source_count + 1
            call require(fact%is_refused .and. .not. fact%is_dynamic_type_known .and. &
                len_trim(fact%dynamic_type) == 0, &
                'polymorphic source was assigned a guessed dynamic type')
        else if (fact%has_unresolved_alias) then
            alias_count = alias_count + 1
            call require(fact%is_refused .and. .not. fact%is_replayable, &
                'target component assignment was not refused')
        else if (fact%has_global_mutable_state) then
            global_count = global_count + 1
            call require(fact%is_refused .and. .not. fact%is_replayable, &
                'global component assignment was not refused')
        else if (fact%has_control_flow_boundary) then
            control_count = control_count + 1
            call require(fact%is_refused .and. .not. fact%is_replayable, &
                'control-flow assignment was not refused')
        end if
    end do
    call require(assignment_count >= 2 .and. found_replayable_assignment .and. &
        replayable_count == 1 .and. polymorphic_source_count == 1 .and. &
        alias_count == 1 .and. global_count == 1 .and. control_count == 1, &
        'polymorphic assignment/refusal ownership events are incomplete')

    ! Independent behavioral oracle: GNU executes the same intrinsic
    ! assignment and checks dynamic type plus deep-copy isolation.
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o ' // executable // ' ' // fixture, wait=.true., exitstat=status)
    call require(status == 0, 'GNU rejected the polymorphic assignment fixture')
    call execute_command_line(executable, wait=.true., exitstat=status)
    call test_remove_file(executable)
    call require(status == 0, &
        'runtime semantic oracle rejected polymorphic assignment behavior')

    print *, 'PASS: polymorphic assignment replay API oracle'

contains

    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_polymorphic_assignment_replay
