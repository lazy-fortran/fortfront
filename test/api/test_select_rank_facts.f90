program test_select_rank_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_RANK, &
        CONTROL_TYPE_GUARD, &
        SELECT_RANK_DISPATCH_EXPLICIT, SELECT_RANK_DISPATCH_ASSUMED_SIZE, &
        SELECT_RANK_DISPATCH_DEFAULT
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count
    logical :: class_default_seen

    call read_example('examples/f90/select_rank_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'SELECT RANK facts fixture was rejected')

    select_count = 0
    class_default_seen = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'type_guard_block') then
            query = query_control_statement(result%arena, i)
            if (query%statement_kind == CONTROL_TYPE_GUARD .and. &
                query%is_default .and. trim(query%guard_type) == 'class_default') then
                class_default_seen = .true.
            end if
            cycle
        end if
        if (trim(get_node_type_at(result%arena, i)) /= 'select_rank') cycle
        select_count = select_count + 1
        query = query_control_statement(result%arena, i)
        call check_select_rank_query(query, select_count)
    end do
    call require(select_count == 2, 'SELECT RANK construct count is wrong')
    call require(class_default_seen, 'CLASS DEFAULT boundary was not preserved')

    query = query_control_statement(result%arena, 0)
    call require(.not. query%found, 'invalid control query unexpectedly resolved')
    call require(allocated(query%rank_arms), &
        'invalid control query did not initialize arm facts')
    if (allocated(query%rank_arms)) then
        call require(size(query%rank_arms) == 0, &
            'invalid control query crossed its boundary')
    end if

    print *, 'PASS: SELECT RANK facts contract'

contains

    include '../common/read_example.inc'

    subroutine check_select_rank_query(query, ordinal)
        type(control_statement_query_t), intent(in) :: query
        integer, intent(in) :: ordinal

        call require(query%found, 'SELECT RANK query did not resolve')
        call require(query%statement_kind == CONTROL_SELECT_RANK, &
            'wrong SELECT RANK statement kind')
        call require(query%has_selector .and. query%selector_node_index > 0, &
            'SELECT RANK selector identity is absent')
        call require(size(query%rank_arms) == 3 .or. size(query%rank_arms) == 2, &
            'SELECT RANK arm count is wrong')
        if (ordinal == 1) then
            call require(size(query%rank_arms) == 3, &
                'explicit selector arm count is wrong')
            call require(query%rank_arms(1)%has_rank .and. &
                query%rank_arms(1)%selected_rank == 0 .and. &
                query%rank_arms(1)%dispatch_kind == SELECT_RANK_DISPATCH_EXPLICIT, &
                'rank zero dispatch facts are wrong')
            call require(query%rank_arms(2)%has_rank .and. &
                query%rank_arms(2)%selected_rank == 1, &
                'rank one dispatch facts are wrong')
            call require(query%rank_arms(3)%is_default .and. &
                query%rank_arms(3)%dispatch_kind == SELECT_RANK_DISPATCH_DEFAULT, &
                'default dispatch facts are wrong')
            call require(query%rank_arms(1)%selector_declaration_index > 0 .and. &
                query%rank_arms(1)%is_storage_resolved .and. &
                .not. query%rank_arms(1)%is_refusal_boundary, &
                'resolved selector identity boundary is wrong')
            call require(query%rank_arms(1)%source_boundary_known .and. &
                query%rank_arms(1)%dispatch_boundary_known .and. &
                query%rank_arms(1)%body_entry_node_index > 0, &
                'source/dispatch boundary facts are missing')
        else
            call require(size(query%rank_arms) == 2, &
                'pointer selector arm count is wrong')
            call require(query%rank_arms(1)%is_pointer_selector .and. &
                query%rank_arms(1)%is_dynamic_ownership_unresolved .and. &
                query%rank_arms(1)%is_refusal_boundary .and. &
                index(query%rank_arms(1)%refusal_reason, 'pointer') > 0, &
                'pointer ownership boundary was hidden')
            call require(query%rank_arms(2)%is_assumed_size .and. &
                query%rank_arms(2)%dispatch_kind == SELECT_RANK_DISPATCH_ASSUMED_SIZE, &
                'assumed-size dispatch facts are wrong')
        end if
    end subroutine check_select_rank_query

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_select_rank_facts
