program test_type_bound_generic_dispatch_query
    use test_command_helpers, only: test_executable_path, test_remove_file
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, subroutine_call_node, &
        type_bound_generic_dispatch_query_t, &
        query_type_bound_generic_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_result_t) :: ambiguous_result
    type(type_bound_generic_dispatch_query_t) :: query
    character(len=:), allocatable :: source, ambiguous_source, executable
    integer :: i, call_count, resolved_count, status
    logical :: saw_integer, saw_real, saw_named_pass
    logical :: saw_alias, saw_global, saw_dynamic, saw_ambiguous

    call read_example('examples/f90/type_bound_generic_dispatch_query.f90', &
        source)
    executable = test_executable_path('fortfront_type_bound_generic_dispatch_query')
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/type_bound_generic_dispatch_query.f90', wait=.true., &
        exitstat=status)
    call require(status == 0, 'GNU Fortran rejected generic dispatch fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=status)
    call require(status == 0, 'generic dispatch runtime oracle failed')
    call test_remove_file(executable)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'generic dispatch fixture was rejected: '// &
        trim(result%diagnostic_text))

    call_count = 0
    resolved_count = 0
    saw_integer = .false.
    saw_real = .false.
    saw_named_pass = .false.
    saw_alias = .false.
    saw_global = .false.
    saw_dynamic = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'subroutine_call') cycle
        select type (node => result%arena%entries(i)%node)
            type is (subroutine_call_node)
            if (.not. allocated(node%name)) cycle
            if (index(trim(node%name), '%choose') <= 0) cycle
        end select
        call_count = call_count + 1
        query = query_type_bound_generic_dispatch(result%arena, i)
        if (query%has_unresolved_alias) then
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%selected_procedure_node_index == 0, &
                'alias boundary was not refused')
        else if (query%has_global_mutable_state) then
            saw_global = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%selected_procedure_node_index == 0, &
                'global-state boundary was not refused')
        else if (query%has_dynamic_receiver) then
            saw_dynamic = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%has_allocatable_boundary .and. &
                query%selected_procedure_node_index == 0, &
                'dynamic receiver boundary was not refused')
        else
            resolved_count = resolved_count + 1
            call require(query%found .and. query%is_resolved .and. &
                .not. query%is_refused .and. query%is_generic_binding .and. &
                query%selected_candidate_index > 0 .and. &
                query%signature%found, 'generic target was not resolved')
            if (trim(query%receiver_name) == 'object' .and. &
                trim(query%candidates(query%selected_candidate_index)% &
                procedure_name) == 'choose_int') then
                saw_integer = .true.
                call require(size(query%candidates) == 2, &
                    'integer generic candidate set is incomplete')
                call require(trim(query%selected_pass_name) == 'self' .and. &
                    query%selected_pass_position == 1, &
                    'integer PASS position was not exposed')
            else if (trim(query%receiver_name) == 'object' .and. &
                    trim(query%candidates(query%selected_candidate_index)% &
                    procedure_name) == 'choose_real') then
                saw_real = .true.
                call require(size(query%candidates) == 2, &
                    'real generic candidate set is incomplete')
                call require(trim(query%selected_pass_name) == 'self' .and. &
                    query%selected_pass_position == 1, &
                    'real PASS position was not exposed')
            else if (trim(query%receiver_name) == 'object' .and. &
                    trim(query%candidates(query%selected_candidate_index)% &
                    procedure_name) == 'choose_named') then
                saw_named_pass = .true.
                call require(size(query%candidates) == 1, &
                    'named PASS generic candidate set is incomplete')
                call require(query%selected_pass_position == 2 .and. &
                    query%signature%dummy_count == 2 .and. &
                    trim(query%signature%dummies(1)%name) == 'value' .and. &
                    trim(query%signature%dummies(2)%name) == 'self', &
                    'named PASS metadata was not exposed')
            end if
        end if
    end do

    call require(call_count == 6 .and. resolved_count == 3 .and. &
        saw_integer .and. saw_real .and. saw_named_pass .and. saw_alias .and. &
        saw_global .and. saw_dynamic, 'generic dispatch coverage is incomplete')

    call read_example('examples/f90/type_bound_generic_dispatch_ambiguous.f90', &
        ambiguous_source)
    options%run_semantics = .false.
    call compile_frontend_from_string(ambiguous_source, ambiguous_result, options)
    call require(ambiguous_result%parse_ok, 'ambiguous fixture did not parse')
    saw_ambiguous = .false.
    do i = 1, ambiguous_result%arena%size
        if (.not. ambiguous_result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(ambiguous_result%arena, i)) /= &
            'subroutine_call') cycle
        query = query_type_bound_generic_dispatch(ambiguous_result%arena, i)
        if (.not. query%found) cycle
        if (query%is_ambiguous) then
            saw_ambiguous = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%selected_procedure_node_index == 0, &
                'ambiguous generic was not refused')
        end if
    end do
    call require(saw_ambiguous, 'dynamic generic ambiguity was not exposed')
    print *, 'PASS: type-bound generic dispatch query contract'

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

end program test_type_bound_generic_dispatch_query
