program test_select_type_associate_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, get_identifier_name
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: query
    character(len=:), allocatable :: source, target_name, error_message
    integer :: i, select_count
    logical :: saw_associate, saw_direct

    call read_example('examples/f90/select_type_associate_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'SELECT TYPE associate fixture was rejected')

    select_count = 0
    saw_associate = .false.
    saw_direct = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        query = query_control_statement(result%arena, i)
        call require(query%found .and. query%statement_kind == CONTROL_SELECT_TYPE, &
            'SELECT TYPE control facts were not exposed')
        call require(size(query%type_arms) == 2, &
            'SELECT TYPE arm count is wrong')
        if (query%type_arms(1)%is_selector_associate) then
            saw_associate = .true.
            call require(trim(query%type_arms(1)%selector_associate_name) == 'typed', &
                'SELECT TYPE associate name is wrong')
            call require(query%type_arms(1)%selector_associate_node_index > 0 .and. &
                query%type_arms(1)%selector_expression_node_index > 0 .and. &
                query%type_arms(1)%selector_expression_node_index /= &
                query%type_arms(1)%selector_node_index, &
                'SELECT TYPE associate AST identities are incomplete')
            call get_identifier_name(result%arena, &
                query%type_arms(1)%selector_expression_node_index, target_name, &
                error_message)
            call require(len_trim(error_message) == 0 .and. trim(target_name) == 'box', &
                'SELECT TYPE associate target identity is wrong')
            call require(query%type_arms(1)%selector_declaration_index > 0 .and. &
                query%type_arms(1)%selector_storage%found .and. &
                query%type_arms(1)%selector_storage%is_polymorphic, &
                'SELECT TYPE associate target storage facts are incomplete')
            call require(trim(query%type_arms(1)%selector_name) == 'box', &
                'SELECT TYPE target name was not preserved')
        else
            saw_direct = .true.
            call require(.not. query%type_arms(1)%is_selector_associate .and. &
                query%type_arms(1)%selector_expression_node_index == &
                query%type_arms(1)%selector_node_index .and. &
                len_trim(query%type_arms(1)%selector_associate_name) == 0, &
                'direct SELECT TYPE selector facts changed')
        end if
    end do

    call require(select_count == 2 .and. saw_associate .and. saw_direct, &
        'SELECT TYPE selector forms were not enumerated exactly once')
    print *, 'PASS: SELECT TYPE associate selector facts contract'

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

end program test_select_type_associate_facts
