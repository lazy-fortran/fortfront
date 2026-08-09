program test_abstract_select_type_dispatch_contract
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_branch_query_t, query_select_type_branch, &
        select_type_dispatch_query_t, query_select_type_dispatch, &
        type_bound_call_query_t, query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(control_statement_query_t) :: control
    type(select_type_branch_query_t) :: branch
    type(select_type_dispatch_query_t) :: dispatch
    type(type_bound_call_query_t) :: call_facts
    character(len=:), allocatable :: source, syntax_command
    integer :: syntax_status, syntax_exitstat
    integer :: i, j, select_index, root_index, middle_index, leaf_index
    integer :: class_arm, type_arm, class_call, type_call

    call read_example( &
        'examples/f90/abstract_select_type_dispatch_contract.f90', source)
    syntax_command = 'gfortran -fsyntax-only examples/f90/'// &
        'abstract_select_type_dispatch_contract.f90'
    call execute_command_line(syntax_command, wait=.true., &
        exitstat=syntax_exitstat, cmdstat=syntax_status)
    call require(syntax_status == 0 .and. syntax_exitstat == 0, &
        'GNU Fortran rejected the concrete abstract-hierarchy fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'abstract SELECT TYPE fixture did not parse')

    root_index = 0
    middle_index = 0
    leaf_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        select case (trim(derived%name))
        case ('root_t')
            root_index = i
        case ('middle_t')
            middle_index = i
        case ('leaf_t')
            leaf_index = i
        end select
    end do
    call require(root_index > 0 .and. middle_index > 0 .and. leaf_index > 0, &
        'abstract hierarchy types are missing')

    class_arm = 0
    type_arm = 0
    class_call = 0
    type_call = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_index = i
        control = query_control_statement(result%arena, i)
        call require(control%found .and. &
            control%statement_kind == CONTROL_SELECT_TYPE .and. &
            size(control%type_arms) == 1, &
            'concrete SELECT TYPE arm was not exposed')
        j = control%type_arms(1)%arm_node_index
        branch = query_select_type_branch(result%arena, j)
        call require(branch%found .and. branch%is_resolved .and. &
            branch%is_declared_type_relation_known .and. &
            branch%is_guard_extension_of_declared .and. &
            trim(branch%declared_type_name) == 'root_t' .and. &
            trim(branch%guard_type_name) == 'leaf_t', &
            'two-level guard hierarchy facts are wrong')
        if (branch%is_class_is) class_arm = j
        if (branch%is_type_is) type_arm = j
        j = control%type_arms(1)%body_node_indices(1)
        call_facts = query_type_bound_call(result%arena, j)
        call require(call_facts%found .and. &
            trim(call_facts%binding_name) == 'work' .and. &
            trim(call_facts%receiver_name) == 'object', &
            'concrete arm call was not exposed')
        dispatch = query_select_type_dispatch(result%arena, &
            control%type_arms(1)%arm_node_index, j)
        call require(dispatch%select_type_node_index == select_index, &
            'concrete dispatch select boundary is wrong')
        call require(dispatch%is_resolved .and. .not. dispatch%is_refused .and. &
            .not. dispatch%is_unresolved, 'concrete leaf dispatch was refused')
        call require(dispatch%concrete_type_index == leaf_index .and. &
            dispatch%resolved_type_index == leaf_index .and. &
            dispatch%declaring_type_index == middle_index .and. &
            dispatch%is_inherited .and. &
            trim(dispatch%declaring_type_name) == 'middle_t' .and. &
            trim(dispatch%implementation) == 'middle_work' .and. &
            dispatch%implementation_node_index > 0, &
            'declaring, inherited, or implementation facts are wrong')
        call require(dispatch%dispatch_boundary_known .and. &
            dispatch%arm_entry_node_index == dispatch%call_node_index .and. &
            dispatch%arm_exit_node_index == dispatch%call_node_index, &
            'concrete dispatch boundary was not preserved')
        if (branch%is_class_is) class_call = j
        if (branch%is_type_is) type_call = j
    end do
    call require(class_arm > 0 .and. type_arm > 0 .and. &
        class_call > 0 .and. type_call > 0, &
        'CLASS IS and TYPE IS concrete calls were not distinguished')

    call check_refusals()
    print *, 'PASS: abstract SELECT TYPE dispatch contract'

contains

    include '../common/read_example.inc'

    subroutine check_refusals()
        character(len=:), allocatable :: refusal_source
        type(compiler_frontend_result_t) :: refusal_result
        integer :: arm, call_node

        refusal_source = &
            'module dispatch_refusals'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  type, abstract :: base_t'//new_line('a')// &
            '  contains'//new_line('a')// &
            '    procedure(run_interface), deferred :: run'//new_line('a')// &
            '    procedure, nopass :: first'//new_line('a')// &
            '    procedure, nopass :: second'//new_line('a')// &
            '    generic :: ambiguous => first, second'//new_line('a')// &
            '  end type base_t'//new_line('a')// &
            '  type, abstract, extends(base_t) :: middle_t'//new_line('a')// &
            '  end type middle_t'//new_line('a')// &
            '  type, extends(middle_t) :: leaf_t'//new_line('a')// &
            '  contains'//new_line('a')// &
            '    procedure :: run => leaf_run'//new_line('a')// &
            '  end type leaf_t'//new_line('a')// &
            '  type :: unrelated_t'//new_line('a')// &
            '  contains'//new_line('a')// &
            '    procedure :: run => unrelated_run'//new_line('a')// &
            '  end type unrelated_t'//new_line('a')// &
            '  abstract interface'//new_line('a')// &
            '    subroutine run_interface(self)'//new_line('a')// &
            '      import base_t'//new_line('a')// &
            '      class(base_t) :: self'//new_line('a')// &
            '    end subroutine run_interface'//new_line('a')// &
            '  end interface'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine inspect(value)'//new_line('a')// &
            '    class(base_t), intent(inout) :: value'//new_line('a')// &
            '    select type (value)'//new_line('a')// &
            '    type is (middle_t)'//new_line('a')// &
            '      call value%run()'//new_line('a')// &
            '    class is (unrelated_t)'//new_line('a')// &
            '      call value%run()'//new_line('a')// &
            '    class default'//new_line('a')// &
            '      call value%run()'//new_line('a')// &
            '    end select'//new_line('a')// &
            '  end subroutine inspect'//new_line('a')// &
            '  subroutine inspect_pointer(value)'//new_line('a')// &
            '    class(base_t), pointer, intent(inout) :: value'//new_line('a')// &
            '    select type (value)'//new_line('a')// &
            '    type is (leaf_t)'//new_line('a')// &
            '      call value%run()'//new_line('a')// &
            '    end select'//new_line('a')// &
            '  end subroutine inspect_pointer'//new_line('a')// &
            '  subroutine inspect_owned(value)'//new_line('a')// &
            '    class(base_t), allocatable, intent(inout) :: value'//new_line('a')// &
            '    select type (value)'//new_line('a')// &
            '    type is (leaf_t)'//new_line('a')// &
            '      call value%run()'//new_line('a')// &
            '    end select'//new_line('a')// &
            '  end subroutine inspect_owned'//new_line('a')// &
            '  subroutine leaf_run(self)'//new_line('a')// &
            '    class(leaf_t) :: self'//new_line('a')// &
            '  end subroutine leaf_run'//new_line('a')// &
            '  subroutine unrelated_run(self)'//new_line('a')// &
            '    type(unrelated_t) :: self'//new_line('a')// &
            '  end subroutine unrelated_run'//new_line('a')// &
            '  subroutine first()'//new_line('a')// &
            '  end subroutine first'//new_line('a')// &
            '  subroutine second()'//new_line('a')// &
            '  end subroutine second'//new_line('a')// &
            'end module dispatch_refusals'//new_line('a')

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        call compile_frontend_from_string(refusal_source, refusal_result, options)
        call require(refusal_result%parse_ok, 'refusal fixture did not parse')
        call require_refusal(refusal_result, 'middle_t', 'run', .true., &
            .false., .false.)
        call require_refusal(refusal_result, 'unrelated_t', 'run', .false., &
            .false., .true.)
        call require_refusal(refusal_result, 'leaf_t', 'run', .false., .true., &
            .false.)
    end subroutine check_refusals

    subroutine require_refusal(refusal_result, guard_name, binding_name, &
            deferred, pointer_or_owned, out_of_hierarchy)
        type(compiler_frontend_result_t), intent(in) :: refusal_result
        character(len=*), intent(in) :: guard_name, binding_name
        logical, intent(in) :: deferred, pointer_or_owned, out_of_hierarchy
        type(control_statement_query_t) :: control
        type(select_type_branch_query_t) :: branch
        type(select_type_dispatch_query_t) :: dispatch
        integer :: i, j, arm, call_node
        logical :: found

        found = .false.
        do i = 1, refusal_result%arena%size
            if (.not. refusal_result%arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(refusal_result%arena, i)) /= 'select_type') cycle
            control = query_control_statement(refusal_result%arena, i)
            do j = 1, size(control%type_arms)
                branch = query_select_type_branch(refusal_result%arena, &
                    control%type_arms(j)%arm_node_index)
                if (trim(branch%guard_type_name) /= guard_name) cycle
                arm = control%type_arms(j)%arm_node_index
                if (.not. allocated(control%type_arms(j)%body_node_indices)) cycle
                if (size(control%type_arms(j)%body_node_indices) /= 1) cycle
                call_node = control%type_arms(j)%body_node_indices(1)
                dispatch = query_select_type_dispatch( &
                    refusal_result%arena, arm, call_node)
                if (trim(dispatch%binding_name) /= binding_name) cycle
                if (deferred .and. .not. dispatch%is_deferred_binding) cycle
                if (pointer_or_owned .and. &
                    .not. dispatch%is_ownership_changing) cycle
                if (out_of_hierarchy .and. .not. branch%is_out_of_hierarchy) cycle
                call require(dispatch%is_refused .and. &
                    .not. dispatch%is_resolved .and. &
                    dispatch%implementation_node_index == 0, &
                    'refusal exposed a callable implementation for '// &
                    trim(guard_name))
                call require(len_trim(dispatch%refusal_reason) > 0, &
                    'refusal reason lost for '//trim(guard_name))
                found = .true.
                if (found) exit
            end do
            if (found) exit
        end do
        call require(found, 'expected refusal was not exposed for '//trim(guard_name))
    end subroutine require_refusal

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_abstract_select_type_dispatch_contract
