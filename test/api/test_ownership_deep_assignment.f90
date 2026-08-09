program test_ownership_deep_assignment
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, get_identifier_name, &
        ownership_event_query_t, query_ownership_events, &
        OWNERSHIP_EVENT_ASSIGNMENT, OWNERSHIP_ASSIGNMENT_DEEP_DERIVED
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    character(:), allocatable :: source, lhs_name, error_message
    integer :: i, module_index, deep_count, refused_global_count
    integer :: refused_alias_count, status

    call read_example('examples/f90/ownership_deep_assignment_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'deep-assignment fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/ownership_deep_assignment_facts.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the deep-assignment fixture')

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
    deep_count = 0
    refused_global_count = 0
    refused_alias_count = 0
    do i = 1, size(events)
        if (events(i)%event_kind /= OWNERSHIP_EVENT_ASSIGNMENT) cycle
        call require(events(i)%is_deep_assignment .and. &
            events(i)%has_owned_components, &
            'derived assignment did not report owned components')
        call require(events(i)%lhs_owner_path%base_node_index > 0 .and. &
            events(i)%rhs_owner_path%base_node_index > 0, &
            'deep-assignment operand paths are missing')
        call get_identifier_name(result%arena, &
            events(i)%lhs_owner_path%base_node_index, lhs_name, error_message)
        select case (trim(lhs_name))
        case ('lhs')
            deep_count = deep_count + 1
            call require(events(i)%assignment_kind == &
                OWNERSHIP_ASSIGNMENT_DEEP_DERIVED .and. &
                events(i)%reallocation_kind == 0 .and. &
                .not. events(i)%is_refused, &
                'local deep assignment was not resolved safely')
        case ('shared')
            refused_global_count = refused_global_count + 1
            call require(events(i)%has_global_mutable_state .and. &
                events(i)%is_refused, &
                'global mutable state was not an explicit refusal')
        case ('alias_lhs')
            refused_alias_count = refused_alias_count + 1
            call require(events(i)%has_unresolved_alias .and. &
                events(i)%is_refused, &
                'unsafe alias was not an explicit refusal')
        case default
            call require(.false., 'unexpected deep-assignment destination')
        end select
    end do

    call require(deep_count == 1 .and. refused_global_count == 1 .and. &
        refused_alias_count == 1, &
        'deep-assignment facts did not cover local and refusal cases')
    print *, 'PASS: deep-assignment ownership facts API oracle'

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

end program test_ownership_deep_assignment
