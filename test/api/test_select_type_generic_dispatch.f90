program test_select_type_generic_dispatch
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_generic_dispatch_query_t, &
        query_select_type_generic_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_generic_dispatch_query_t) :: generic_query
    character(len=:), allocatable :: source
    integer :: i, select_index, call_count, resolved_count
    logical :: saw_ambiguous, saw_pointer, saw_allocatable

    call read_example( &
        'examples/f90/select_type_generic_dispatch.f90', source)

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'generic dispatch fixture was rejected: '// &
        trim(result%diagnostic_text))

    select_index = 0
    call_count = 0
    resolved_count = 0
    saw_ambiguous = .false.
    saw_pointer = .false.
    saw_allocatable = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'select_type') then
            select_index = i
            control = query_control_statement(result%arena, i)
            call require(control%found .and. &
                control%statement_kind == CONTROL_SELECT_TYPE, &
                'SELECT TYPE control facts were not exposed')
        end if
    end do
    call require(select_index > 0, 'SELECT TYPE fixture was not found')

    do i = 1, result%arena%size
        if (trim(get_node_type_at(result%arena, i)) /= 'subroutine_call') cycle
        call_count = call_count + 1
        generic_query = query_select_type_generic_dispatch(result%arena, &
            arm_for_call(result%arena, i), i)
        if (generic_query%is_resolved) then
            resolved_count = resolved_count + 1
            call require(generic_query%is_generic_binding .and. &
                .not. generic_query%is_ambiguous .and. &
                size(generic_query%candidates) == 2 .and. &
                generic_query%selected_candidate_index > 0 .and. &
                generic_query%signature%found .and. &
                generic_query%signature%dummy_count == 2 .and. &
                trim(generic_query%signature%dummies(1)%name) == 'self' .and. &
                trim(generic_query%signature%dummies(2)%name) == 'value', &
                'unique generic candidate facts are incomplete')
            call require(count_matches(generic_query) == 1, &
                'generic candidate oracle found more than one match')
            if (resolved_count == 1) then
                call require(trim(generic_query%candidates( &
                    generic_query%selected_candidate_index)%procedure_name) == &
                    'choose_int', 'integer actual selected the wrong specific')
            else if (resolved_count == 2) then
                call require(trim(generic_query%candidates( &
                    generic_query%selected_candidate_index)%procedure_name) == &
                    'choose_real', 'real actual selected the wrong specific')
            end if
        else if (generic_query%is_pointer_boundary) then
            saw_pointer = .true.
            call require(generic_query%is_refused .and. &
                generic_query%is_unresolved, &
                'pointer generic receiver was not explicitly refused')
        else if (generic_query%is_allocatable_boundary) then
            saw_allocatable = .true.
            call require(generic_query%is_refused .and. &
                generic_query%is_unresolved, &
                'allocatable generic receiver was not explicitly refused')
        else if (generic_query%is_ambiguous) then
            saw_ambiguous = .true.
            call require(generic_query%is_refused .and. &
                generic_query%is_unresolved, &
                'ambiguous generic was not explicitly refused')
        end if
    end do

    call require(call_count == 5, 'unexpected type-bound call count')
    call require(resolved_count == 2, &
        'integer and real generic specifics were not resolved independently')
    call require(saw_ambiguous, 'ambiguous generic refusal was not observed')
    call require(saw_pointer .and. saw_allocatable, &
        'pointer and allocatable generic boundaries were not observed')
    print *, 'PASS: narrowed SELECT TYPE generic dispatch contract'

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

    integer function arm_for_call(arena, call_index) result(arm_index)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        type(control_statement_query_t) :: local_control
        integer :: i, j, k

        arm_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(arena, i)) /= 'select_type') cycle
            local_control = query_control_statement(arena, i)
            do j = 1, size(local_control%type_arms)
                if (.not. allocated(local_control%type_arms(j)%body_node_indices)) &
                    cycle
                do k = 1, size(local_control%type_arms(j)%body_node_indices)
                    if (local_control%type_arms(j)%body_node_indices(k) == &
                            call_index) then
                        arm_index = local_control%type_arms(j)%arm_node_index
                        return
                    end if
                end do
            end do
        end do
    end function arm_for_call

    integer function count_matches(query) result(count)
        type(select_type_generic_dispatch_query_t), intent(in) :: query
        integer :: j

        count = 0
        do j = 1, size(query%candidates)
            if (query%candidates(j)%is_match) count = count + 1
        end do
    end function count_matches

end program test_select_type_generic_dispatch
