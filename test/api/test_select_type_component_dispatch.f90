program test_select_type_component_dispatch
    use fortfront, only: ast_arena_t, compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, get_subroutine_body_info, &
        control_statement_query_t, query_control_statement, &
        select_type_component_dispatch_query_t, &
        query_select_type_component_dispatch
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_component_dispatch_query_t) :: query
    character(len=:), allocatable :: source, procedure_name
    integer :: i, arm, select_index, call_count
    logical :: saw_supported, saw_generic, saw_pointer, saw_allocatable
    logical :: saw_global, saw_alias, saw_nested, saw_missing
    logical :: saw_section, saw_stride, saw_dynamic, saw_rank2

    call read_example('examples/f90/select_type_component_dispatch.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'component dispatch fixture did not parse: '// &
        trim(result%error_msg))

    select_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_index = i
        control = query_control_statement(result%arena, i)
        call require(control%found .and. size(control%type_arms) == 1, &
            'component dispatch SELECT TYPE arm facts are incomplete')
    end do
    call require(select_index > 0, 'component dispatch SELECT TYPE is absent')

    call_count = 0
    saw_supported = .false.
    saw_generic = .false.
    saw_pointer = .false.
    saw_allocatable = .false.
    saw_global = .false.
    saw_alias = .false.
    saw_nested = .false.
    saw_missing = .false.
    saw_section = .false.
    saw_stride = .false.
    saw_dynamic = .false.
    saw_rank2 = .false.

    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'subroutine_call') cycle
        arm = arm_for_call(result%arena, i)
        if (arm <= 0) cycle
        query = query_select_type_component_dispatch(result%arena, arm, i)
        procedure_name = enclosing_subroutine(result%arena, i)
        call_count = call_count + 1
        select case (trim(procedure_name))
        case ('inspect_supported')
            saw_supported = .true.
            call require(query%found .and. query%is_resolved .and. &
                .not. query%is_refused .and. .not. query%is_unresolved, &
                'direct component binding was not resolved')
            call require(query%is_type_is .and. query%is_inherited .and. &
                trim(query%receiver_name) == 'typed%leaf' .and. &
                trim(query%component_type_name) == 'component_leaf_t' .and. &
                trim(query%declaring_type_name) == 'component_mid_t' .and. &
                trim(query%implementation) == 'mid_run', &
                'component dispatch identity/provenance is wrong')
            call require(query%signature%found .and. &
                query%signature%dummy_count == 2 .and. &
                trim(query%signature%dummies(1)%name) == 'self' .and. &
                trim(query%signature%dummies(2)%name) == 'amount' .and. &
                query%pass_arg .and. query%implementation_pass_position == 1 .and. &
                trim(query%implementation_pass_name) == 'self', &
                'component dispatch signature/PASS facts are incomplete')
            call require(query%receiver_path%found .and. &
                size(query%receiver_path%component_names) == 1 .and. &
                trim(query%receiver_path%component_names(1)) == 'leaf', &
                'component receiver path was not preserved')
        case ('inspect_section')
            saw_section = .true.
            call require(query%found .and. query%is_resolved .and. &
                .not. query%is_refused .and. .not. query%is_unresolved, &
                'literal contiguous component section was not resolved')
            call require(query%is_array_receiver .and. &
                query%is_array_section_receiver .and. &
                query%is_literal_array_section .and. &
                query%is_contiguous_array_section .and. &
                query%array_section_rank == 1 .and. &
                query%array_section_lower_bound == 2 .and. &
                query%array_section_upper_bound == 4 .and. &
                query%array_section_stride == 1, &
                'component section shape facts are incomplete')
            call require(trim(query%receiver_name) == &
                'typed%leaf_section(2:4)' .and. &
                trim(query%component_type_name) == 'component_leaf_t' .and. &
                trim(query%implementation) == 'mid_run' .and. &
                query%is_inherited, 'component section dispatch identity is wrong')
            call require(query%receiver_path%found, &
                'component section receiver path was not preserved')
            call require(size(query%receiver_path%component_names) == 1, &
                'component section receiver path has the wrong length')
            call require(trim(query%receiver_path%component_names(1)) == &
                'leaf_section' .and. query%receiver_path%is_array_section .and. &
                query%receiver_path%rank == 1, &
                'component section path facts are incomplete')
        case ('inspect_stride')
            saw_stride = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_array_section_receiver .and. &
                query%is_literal_array_section .and. &
                .not. query%is_contiguous_array_section .and. &
                query%array_section_rank == 1 .and. &
                query%array_section_stride == 2, &
                'noncontiguous component section was not refused')
        case ('inspect_dynamic')
            saw_dynamic = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_array_section_receiver .and. &
                .not. query%is_literal_array_section .and. &
                .not. query%is_contiguous_array_section, &
                'dynamic component section was not refused')
        case ('inspect_rank2')
            saw_rank2 = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_array_section_receiver .and. &
                query%array_section_rank == 2, &
                'rank-two component section was not refused')
        case ('inspect_generic')
            saw_generic = .true.
            call require(query%found .and. query%is_generic_binding .and. &
                query%is_refused .and. query%is_unresolved .and. &
                query%implementation_node_index == 0, &
                'generic component binding was guessed')
        case ('inspect_pointer')
            saw_pointer = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_pointer_boundary .and. query%has_unresolved_alias, &
                'pointer component alias boundary was lost')
        case ('inspect_allocatable')
            saw_allocatable = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_allocatable_boundary .and. query%is_ownership_changing, &
                'allocatable component ownership boundary was lost')
        case ('inspect_global')
            saw_global = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%has_global_mutable_state .and. query%is_ownership_changing, &
                'global mutable selector boundary was lost')
        case ('inspect_alias')
            saw_alias = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%has_unresolved_alias, 'ASSOCIATE alias was guessed')
        case ('inspect_nested')
            saw_nested = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%is_nested, 'nested component call was guessed')
        case ('inspect_missing')
            saw_missing = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%implementation_node_index == 0, &
                'unresolved component binding was guessed')
        end select
    end do

    call require(call_count == 12 .and. saw_supported .and. saw_section .and. &
        saw_stride .and. saw_dynamic .and. saw_rank2 .and. saw_generic .and. &
        saw_pointer .and. saw_allocatable .and. saw_global .and. saw_alias .and. &
        saw_nested .and. saw_missing, 'component dispatch cases were incomplete')

    query = query_select_type_component_dispatch(result%arena, 0, 0)
    call require(query%is_refused .and. query%is_unresolved .and. &
        .not. query%is_resolved, 'invalid component dispatch was not refused')
    print *, 'PASS: SELECT TYPE component direct dispatch contract'

contains

    include '../common/read_example.inc'

    integer function arm_for_call(arena, call_index) result(arm_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        type(control_statement_query_t) :: local_control
        integer :: i, j

        arm_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (trim(get_node_type_at(arena, i)) /= 'select_type') cycle
            local_control = query_control_statement(arena, i)
            do j = 1, size(local_control%type_arms)
                if (node_is_under(arena, call_index, &
                    local_control%type_arms(j)%arm_node_index)) then
                    arm_index = local_control%type_arms(j)%arm_node_index
                    return
                end if
            end do
        end do
    end function arm_for_call

    logical function node_is_under(arena, node_index, ancestor_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, ancestor_index
        integer :: current, steps

        node_is_under = .false.
        current = node_index
        steps = 0
        do while (current > 0)
            if (current == ancestor_index) then
                node_is_under = .true.
                return
            end if
            if (.not. arena%has_node_at(current)) return
            current = arena%entries(current)%parent_index
            steps = steps + 1
            if (steps > arena%size) return
        end do
    end function node_is_under

    function enclosing_subroutine(arena, node_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: name
        character(len=:), allocatable :: candidate, error_msg
        integer, allocatable :: params(:), body(:)
        integer :: current

        name = ''
        current = node_index
        do while (current > 0)
            call get_subroutine_body_info(arena, current, candidate, params, &
                body, error_msg)
            if (len_trim(candidate) > 0) then
                name = trim(candidate)
                return
            end if
            current = arena%entries(current)%parent_index
        end do
    end function enclosing_subroutine

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_select_type_component_dispatch
