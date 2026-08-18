program test_abstract_dispatch_runtime_boundary
    use iso_fortran_env, only: error_unit
    use test_command_helpers, only: test_executable_path, test_remove_file
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_branch_query_t, query_select_type_branch, &
        select_type_dispatch_query_t, query_select_type_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_branch_query_t) :: branch
    type(select_type_dispatch_query_t) :: dispatch
    character(len=:), allocatable :: source
    integer :: i, j, select_count, call_node, status
    logical :: saw_inherited, saw_override, saw_abstract
    character(len=*), parameter :: fixture = &
        'examples/f90/abstract_dispatch_runtime_boundary.f90'
    character(len=:), allocatable :: executable

    call read_example(fixture, source)
    executable = test_executable_path('fortfront_abstract_dispatch_runtime_boundary')
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o ' // executable // ' ' // fixture, wait=.true., exitstat=status)
    call require(status == 0, 'GNU rejected abstract dispatch fixture')
    call execute_command_line(executable, wait=.true., exitstat=status)
    call test_remove_file(executable)
    call require(status == 0, &
        'GNU runtime oracle rejected inherited/override dispatch behavior')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), &
        'FortFront rejected abstract dispatch runtime fixture')

    select_count = 0
    saw_inherited = .false.
    saw_override = .false.
    saw_abstract = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        control = query_control_statement(result%arena, i)
        call require(control%found, 'abstract dispatch control facts are missing')
        call require(control%statement_kind == CONTROL_SELECT_TYPE, &
            'abstract dispatch control kind is wrong')
        call require(allocated(control%type_arms), &
            'abstract dispatch arms are missing')
        call require(size(control%type_arms) == 3, &
            'abstract dispatch SELECT TYPE arms are incomplete')
        do j = 1, size(control%type_arms)
            branch = query_select_type_branch(result%arena, &
                control%type_arms(j)%arm_node_index)
            call require(branch%found, 'dispatch branch facts are missing')
            call require(allocated(control%type_arms(j)%body_node_indices), &
                'dispatch arm body is missing')
            call require(size(control%type_arms(j)%body_node_indices) == 1, &
                'dispatch arm is not one direct call')
            call_node = control%type_arms(j)%body_node_indices(1)
            dispatch = query_select_type_dispatch(result%arena, &
                control%type_arms(j)%arm_node_index, call_node)
            select case (trim(branch%guard_type_name))
            case ('inherited_leaf_t')
                saw_inherited = .true.
                call require(dispatch%found .and. dispatch%is_resolved .and. &
                    .not. dispatch%is_refused .and. &
                    trim(dispatch%implementation) == 'root_work' .and. &
                    trim(dispatch%declaring_type_name) == 'root_t' .and. &
                    dispatch%is_inherited .and. &
                    dispatch%implementation_node_index > 0, &
                    'inherited concrete leaf dispatch facts are wrong')
            case ('override_leaf_t')
                saw_override = .true.
                call require(dispatch%found .and. dispatch%is_resolved .and. &
                    .not. dispatch%is_refused .and. &
                    trim(dispatch%implementation) == 'override_work' .and. &
                    trim(dispatch%declaring_type_name) == 'override_leaf_t' .and. &
                    .not. dispatch%is_inherited .and. &
                    dispatch%implementation_node_index > 0, &
                    'direct concrete leaf dispatch facts are wrong')
            case ('middle_t')
                saw_abstract = .true.
                call require(dispatch%found .and. dispatch%is_abstract_guard .and. &
                    dispatch%is_refused .and. dispatch%is_unresolved .and. &
                    .not. dispatch%is_resolved .and. &
                    len_trim(dispatch%implementation) == 0 .and. &
                    dispatch%implementation_node_index == 0, &
                    'abstract guard leaked an unresolved implementation target')
            case default
                call require(.false., 'unexpected abstract dispatch guard')
            end select
        end do
    end do

    call require(select_count == 1 .and. saw_inherited .and. &
        saw_override .and. saw_abstract, &
        'abstract dispatch boundary cases are incomplete')
    print *, 'PASS: abstract dispatch runtime boundary contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_abstract_dispatch_runtime_boundary
