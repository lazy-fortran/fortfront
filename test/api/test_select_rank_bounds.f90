program test_select_rank_bounds
    use test_command_helpers, only: test_executable_path, test_remove_file
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_RANK, &
        SELECT_RANK_DISPATCH_EXPLICIT, SELECT_RANK_DISPATCH_DEFAULT, &
        declaration_query_t, query_declaration, array_bounds_query_t, &
        query_array_bounds
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: query
    type(declaration_query_t) :: declaration
    type(array_bounds_query_t) :: bounds
    character(len=:), allocatable :: source, executable
    integer :: i, select_count, declaration_index, explicit_count, status

    call read_example('examples/f90/select_rank_bounds.f90', source)
    executable = test_executable_path('fortfront_select_rank_bounds')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'SELECT RANK bounds fixture was rejected')

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/select_rank_bounds.f90', wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected SELECT RANK bounds fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=status)
    call require(status == 0, 'SELECT RANK runtime oracle failed')
    call test_remove_file(executable)

    select_count = 0
    explicit_count = 0
    do i = 1, result%arena%size
        query = query_control_statement(result%arena, i)
        if (.not. query%found .or. query%statement_kind /= CONTROL_SELECT_RANK) cycle
        select_count = select_count + 1
        call require(size(query%rank_arms) == 3, &
            'SELECT RANK bounds arm count is wrong')
        call require(query%rank_arms(1)%has_rank .and. &
            query%rank_arms(1)%selected_rank == 1 .and. &
            query%rank_arms(1)%dispatch_kind == SELECT_RANK_DISPATCH_EXPLICIT, &
            'rank-one arm fact is wrong')
        call require(query%rank_arms(2)%has_rank .and. &
            query%rank_arms(2)%selected_rank == 2 .and. &
            query%rank_arms(2)%dispatch_kind == SELECT_RANK_DISPATCH_EXPLICIT, &
            'rank-two arm fact is wrong')
        call require(query%rank_arms(3)%is_default .and. &
            query%rank_arms(3)%dispatch_kind == SELECT_RANK_DISPATCH_DEFAULT, &
            'default arm fact is wrong')
        call require(query%rank_arms(1)%selector_declaration_index > 0 .and. &
            query%rank_arms(1)%is_storage_resolved .and. &
            query%rank_arms(1)%selector_is_assumed_rank .and. &
            query%rank_arms(1)%selector_storage%is_assumed_rank .and. &
            query%rank_arms(1)%selector_storage%rank == -1 .and. &
            .not. query%rank_arms(1)%is_refusal_boundary, &
            'assumed-rank selector storage fact is wrong')
        call require(query%rank_arms(1)%selector_bounds_identity_known .and. &
            size(query%rank_arms(1)%selector_bounds_node_indices) == 1, &
            'assumed-rank selector bounds identity is missing')
        bounds = query_array_bounds(result%arena, &
            query%rank_arms(1)%selector_bounds_node_indices(1))
        call require(bounds%found .and. bounds%is_assumed_rank .and. &
            bounds%lower_bound_node_index == 0 .and. &
            bounds%upper_bound_node_index == 0 .and. &
            bounds%stride_node_index == 0, &
            'assumed-rank bounds fact is not source-backed')
        call require(query%rank_arms(2)%selector_is_assumed_rank .and. &
            query%rank_arms(3)%selector_is_assumed_rank, &
            'bounds identity was not preserved across SELECT RANK arms')
        explicit_count = explicit_count + 2
        declaration_index = query%rank_arms(1)%selector_declaration_index
        declaration = query_declaration(result%arena, declaration_index)
        call require(declaration%found .and. declaration%is_array, &
            'selector declaration facts are missing')
        call require(allocated(declaration%dimension_indices), &
            'selector declaration bounds are missing')
        call require(size(declaration%dimension_indices) > 0, &
            'selector declaration bounds are empty')
        call require(declaration%dimension_indices(1) == &
            query%rank_arms(1)%selector_bounds_node_indices(1), &
            'selector declaration and bounds identities disagree')
    end do

    call require(select_count == 1 .and. explicit_count == 2, &
        'SELECT RANK bounds query coverage is incomplete')
    print *, 'PASS: SELECT RANK bounds API/runtime oracle'

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

end program test_select_rank_bounds
