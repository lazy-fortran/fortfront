program test_select_rank_facts
    use fortfront_ast, only: get_node_type_at
    use fortfront_compiler, only: compile_frontend_from_string, &
        compiler_frontend_options_t, compiler_frontend_result_t, CONTROL_SELECT_RANK, &
        control_statement_query_t, CONTROL_TYPE_GUARD, query_control_statement, &
        SELECT_RANK_DISPATCH_ASSUMED_SIZE, SELECT_RANK_DISPATCH_DEFAULT, &
        SELECT_RANK_DISPATCH_EXPLICIT, use_statement_query_t, query_use_statements
    use fortfront_semantic, only: INPUT_MODE_STANDARD
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

    call check_nested_select_rank()
    print *, 'PASS: SELECT RANK facts contract'

contains

    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'

    subroutine check_nested_select_rank()
        type(compiler_frontend_result_t) :: nested_result
        type(control_statement_query_t) :: nested_query, child_query
        type(use_statement_query_t), allocatable :: uses(:)
        character(len=:), allocatable :: nested_source, executable
        integer :: node_index, arm, nested_count, outer_count, child_index, status

        call read_example('examples/f90/select_rank_nested.f90', nested_source)
        call compile_frontend_from_string(nested_source, nested_result, options)
        call require(nested_result%success(), &
            'nested SELECT RANK fixture was rejected')
        uses = query_use_statements(nested_result%arena)
        call require(size(uses) == 1, 'nested fixture USE dependency was lost')
        call require(trim(uses(1)%module_name) == 'iso_fortran_env', &
            'nested fixture USE dependency was changed')
        nested_count = 0
        outer_count = 0
        do node_index = 1, nested_result%arena%size
            nested_query = query_control_statement(nested_result%arena, node_index)
            if (nested_query%statement_kind /= CONTROL_SELECT_RANK) cycle
            nested_count = nested_count + 1
            call require(size(nested_query%rank_arms) > 0, &
                'nested rank construct has no arms')
            if (nested_query%rank_arms(1)%selector_name /= 'actual') cycle
            outer_count = outer_count + 1
            call require(size(nested_query%rank_arms) == 3, &
                'outer rank arms were not preserved')
            do arm = 1, 2
                child_index = nested_query%rank_arms(arm)%body_entry_node_index
                child_query = query_control_statement(nested_result%arena, child_index)
                call require(child_query%statement_kind == CONTROL_SELECT_RANK, &
                    'nested rank construct is not an outer arm child')
                call require(size(child_query%rank_arms) == 2, &
                    'nested rank arms were not preserved')
                call require(child_query%rank_arms(1)%selected_rank == &
                    nested_query%rank_arms(arm)%selected_rank, &
                    'nested explicit rank was changed')
                call require(child_query%rank_arms(2)%is_default, &
                    'nested default rank arm was lost')
            end do
        end do
        call require(nested_count == 3 .and. outer_count == 1, &
            'nested SELECT RANK tree is incomplete')
        executable = test_executable_path('fortfront_select_rank_nested')
        call execute_command_line('gfortran -std=f2018 -pedantic -o '// &
            executable//' examples/f90/select_rank_nested.f90', &
            wait=.true., exitstat=status)
        call require(status == 0, 'GNU Fortran rejected nested SELECT RANK fixture')
        call execute_command_line(executable, wait=.true., exitstat=status)
        call require(status == 0, 'nested SELECT RANK runtime oracle failed')
        call test_remove_file(executable)
    end subroutine check_nested_select_rank

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
