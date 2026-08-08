program test_select_type_arm_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        SELECT_TYPE_ARM_TYPE_IS, SELECT_TYPE_ARM_CLASS_IS, &
        SELECT_TYPE_ARM_CLASS_DEFAULT
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, select_count

    call read_example('examples/f90/select_type_arm_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'SELECT TYPE facts fixture was rejected')

    select_count = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        query = query_control_statement(result%arena, i)
        call check_query(query, select_count)
    end do
    call require(select_count == 3, 'SELECT TYPE construct count is wrong')

    query = query_control_statement(result%arena, 0)
    call require(.not. query%found, 'invalid control query unexpectedly resolved')
    call require(size(query%type_arms) == 0, &
        'invalid control query crossed its arm boundary')

    print *, 'PASS: SELECT TYPE arm facts contract'

contains

    include '../common/read_example.inc'

    subroutine check_query(query, ordinal)
        type(control_statement_query_t), intent(in) :: query
        integer, intent(in) :: ordinal

        call require(query%found .and. query%statement_kind == CONTROL_SELECT_TYPE, &
            'SELECT TYPE control facts are incomplete')
        call require(query%has_selector .and. query%selector_node_index > 0, &
            'SELECT TYPE selector identity is absent')
        if (ordinal == 1) then
            call require(size(query%type_arms) == 4, &
                'hierarchy SELECT TYPE arm count is wrong')
            call require(query%type_arms(1)%arm_ordinal == 1, 'TYPE IS ordinal')
            call require(query%type_arms(1)%is_type_is .and. &
                query%type_arms(1)%arm_kind == SELECT_TYPE_ARM_TYPE_IS, &
                'TYPE IS kind')
            call require(query%type_arms(1)%dispatch_boundary_known, 'TYPE IS dispatch')
            call require(query%type_arms(1)%is_concrete_type_resolved, 'TYPE IS identity')
            call require(trim(query%type_arms(1)%concrete_type_name) == 'child_t', &
                'TYPE IS child name')
            call require(query%type_arms(1)%is_declared_type_resolved .and. &
                trim(query%type_arms(1)%declared_type_name) == 'base_t', &
                'selector declared type identity')
            call require(query%type_arms(1)%body_entry_node_index > 0 .and. &
                query%type_arms(1)%body_exit_node_index > 0, &
                'TYPE IS body boundaries')
            call require(query%type_arms(2)%is_class_is .and. &
                query%type_arms(2)%arm_kind == SELECT_TYPE_ARM_CLASS_IS .and. &
                query%type_arms(2)%is_concrete_type_resolved .and. &
                trim(query%type_arms(2)%concrete_type_name) == 'base_t', &
                'CLASS IS base facts are wrong')
            call require(query%type_arms(3)%is_out_of_hierarchy .and. &
                query%type_arms(3)%is_unresolved .and. &
                index(query%type_arms(3)%refusal_reason, 'hierarchy') > 0, &
                'out-of-hierarchy guard was guessed')
            call require(query%type_arms(4)%is_class_default .and. &
                query%type_arms(4)%arm_kind == SELECT_TYPE_ARM_CLASS_DEFAULT .and. &
                query%type_arms(4)%source_boundary_known .and. &
                query%type_arms(4)%body_entry_node_index > 0, &
                'CLASS DEFAULT boundary facts are wrong')
        else if (ordinal == 2) then
            call require(size(query%type_arms) == 2, &
                'unlimited selector arm count is wrong')
            call require(query%type_arms(1)%is_intrinsic .and. &
                query%type_arms(1)%is_unresolved .and. &
                index(query%type_arms(1)%refusal_reason, 'intrinsic') > 0, &
                'intrinsic guard was guessed')
            call require(query%type_arms(2)%is_class_default .and. &
                query%type_arms(2)%is_selector_resolved, &
                'unlimited CLASS DEFAULT facts are wrong')
        else
            call require(size(query%type_arms) == 2, &
                'unresolved guard arm count is wrong')
            call require(query%type_arms(1)%is_unresolved .and. &
                .not. query%type_arms(1)%is_concrete_type_resolved .and. &
                index(query%type_arms(1)%refusal_reason, 'unresolved') > 0, &
                'unresolved guard was guessed')
        end if
    end subroutine check_query

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_select_type_arm_facts
