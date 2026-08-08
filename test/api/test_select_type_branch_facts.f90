program test_select_type_branch_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_branch_query_t, query_select_type_branch, &
        SELECT_TYPE_MATCH_UNKNOWN, SELECT_TYPE_MATCH_EXACT, &
        SELECT_TYPE_MATCH_EXTENSION, SELECT_TYPE_MATCH_DEFAULT
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_branch_query_t) :: branch
    character(len=:), allocatable :: source
    integer :: i, j, select_count

    call read_example('examples/f90/select_type_arm_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'SELECT TYPE branch fixture did not parse')

    ! Independent oracle: the fixture deliberately contains a child TYPE IS,
    ! a same-type CLASS IS, an out-of-hierarchy CLASS IS, CLASS DEFAULT, an
    ! intrinsic guard, and an unresolved guard in that source order.
    select_count = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        control = query_control_statement(result%arena, i)
        call require(control%found .and. &
            control%statement_kind == CONTROL_SELECT_TYPE, &
            'SELECT TYPE construct was not queryable')
        do j = 1, size(control%type_arms)
            branch = query_select_type_branch(result%arena, &
                control%type_arms(j)%arm_node_index)
            call require(branch%found .and. &
                branch%select_type_node_index == i .and. &
                branch%arm_ordinal == j, 'branch identity is incomplete')
            select case (select_count)
            case (1)
                call check_hierarchy_arm(branch, j)
            case (2)
                if (j == 1) then
                    call require(branch%is_refused .and. branch%is_unresolved .and. &
                        branch%match_kind == SELECT_TYPE_MATCH_UNKNOWN .and. &
                        .not. branch%is_exact_dynamic_type .and. &
                        index(branch%refusal_reason, 'intrinsic') > 0, &
                        'intrinsic TYPE IS guard was guessed')
                else
                    call require(branch%is_class_default .and. branch%is_resolved .and. &
                        branch%match_kind == SELECT_TYPE_MATCH_DEFAULT .and. &
                        .not. branch%is_extension_dynamic_type, &
                        'unlimited CLASS DEFAULT facts are wrong')
                end if
            case (3)
                if (j == 1) then
                    call require(branch%is_refused .and. branch%is_unresolved .and. &
                        branch%match_kind == SELECT_TYPE_MATCH_UNKNOWN .and. &
                        index(branch%refusal_reason, 'unresolved') > 0, &
                        'unresolved TYPE IS guard was guessed')
                else
                    call require(branch%is_class_default .and. &
                        branch%match_kind == SELECT_TYPE_MATCH_DEFAULT, &
                        'CLASS DEFAULT boundary was not retained')
                end if
            end select
        end do
    end do

    call require(select_count == 3, 'SELECT TYPE construct count is wrong')
    branch = query_select_type_branch(result%arena, 0)
    call require(.not. branch%found .and. branch%is_refused .and. &
        branch%is_unresolved .and. branch%match_kind == SELECT_TYPE_MATCH_UNKNOWN, &
        'invalid branch query was not refused')
    call check_abstract_guard()
    print *, 'PASS: SELECT TYPE branch type facts contract'

contains

    include '../common/read_example.inc'

    subroutine check_hierarchy_arm(branch, ordinal)
        type(select_type_branch_query_t), intent(in) :: branch
        integer, intent(in) :: ordinal

        select case (ordinal)
        case (1)
            call require(branch%is_type_is .and. branch%is_resolved .and. &
                branch%match_kind == SELECT_TYPE_MATCH_EXACT .and. &
                branch%is_exact_dynamic_type .and. &
                trim(branch%guard_type_name) == 'child_t' .and. &
                trim(branch%declared_type_name) == 'base_t' .and. &
                branch%is_guard_extension_of_declared .and. &
                .not. branch%is_guard_same_as_declared, &
                'child TYPE IS narrowing facts are wrong')
        case (2)
            call require(branch%is_class_is .and. branch%is_resolved .and. &
                branch%match_kind == SELECT_TYPE_MATCH_EXTENSION .and. &
                branch%is_extension_dynamic_type .and. &
                branch%is_guard_same_as_declared .and. &
                .not. branch%is_guard_extension_of_declared, &
                'same-type CLASS IS narrowing facts are wrong')
        case (3)
            call require(branch%is_class_is .and. branch%is_refused .and. &
                branch%is_unresolved .and. branch%is_out_of_hierarchy .and. &
                branch%match_kind == SELECT_TYPE_MATCH_EXTENSION .and. &
                branch%is_extension_dynamic_type, &
                'out-of-hierarchy CLASS IS boundary was lost')
        case (4)
            call require(branch%is_class_default .and. branch%is_resolved .and. &
                branch%match_kind == SELECT_TYPE_MATCH_DEFAULT .and. &
                .not. branch%is_exact_dynamic_type .and. &
                .not. branch%is_extension_dynamic_type, &
                'CLASS DEFAULT narrowing facts are wrong')
        case default
            call require(.false., 'unexpected hierarchy arm ordinal')
        end select
    end subroutine check_hierarchy_arm

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

    subroutine check_abstract_guard()
        character(len=:), allocatable :: abstract_source
        type(compiler_frontend_result_t) :: abstract_result
        type(control_statement_query_t) :: abstract_control
        type(select_type_branch_query_t) :: abstract_branch
        integer :: k, select_index

        abstract_source = &
            'module abstract_select_type'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  type, abstract :: base_t'//new_line('a')// &
            '  end type base_t'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine inspect(value)'//new_line('a')// &
            '    class(base_t), intent(in) :: value'//new_line('a')// &
            '    select type (value)'//new_line('a')// &
            '    class is (base_t)'//new_line('a')// &
            '      continue'//new_line('a')// &
            '    class default'//new_line('a')// &
            '      continue'//new_line('a')// &
            '    end select'//new_line('a')// &
            '  end subroutine inspect'//new_line('a')// &
            'end module abstract_select_type'//new_line('a')
        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        call compile_frontend_from_string(abstract_source, abstract_result, options)
        call require(abstract_result%parse_ok, 'abstract guard fixture did not parse')

        select_index = 0
        do k = 1, abstract_result%arena%size
            if (.not. abstract_result%arena%has_node_at(k)) cycle
            if (trim(get_node_type_at(abstract_result%arena, k)) /= 'select_type') cycle
            select_index = k
            exit
        end do
        call require(select_index > 0, 'abstract SELECT TYPE construct is absent')
        abstract_control = query_control_statement(abstract_result%arena, select_index)
        abstract_branch = query_select_type_branch(abstract_result%arena, &
            abstract_control%type_arms(1)%arm_node_index)
        call require(abstract_branch%is_class_is .and. abstract_branch%is_resolved .and. &
            abstract_branch%is_guard_type_abstract .and. &
            abstract_branch%is_guard_same_as_declared .and. &
            abstract_branch%match_kind == SELECT_TYPE_MATCH_EXTENSION, &
            'abstract CLASS IS guard facts are wrong')
    end subroutine check_abstract_guard

end program test_select_type_branch_facts
