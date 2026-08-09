program test_abstract_dispatch_depth_query
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, binding_resolution_query_t, &
        query_type_binding_resolution, type_bound_call_query_t, &
        query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(binding_resolution_query_t) :: binding
    type(type_bound_call_query_t) :: call_binding
    character(len=:), allocatable :: source
    character(len=:), allocatable :: syntax_command
    integer :: syntax_status, syntax_exitstat
    integer :: i, root_index, middle_index, late_index, leaf_index
    integer :: deep_leaf_index, local_index, generic_index
    integer :: local_target, leaf_target, deep_target
    logical :: found_call

    call read_example('examples/f90/abstract_dispatch_depth_query.f90', source)
    syntax_command = 'gfortran -fsyntax-only examples/f90/'// &
        'abstract_dispatch_depth_query.f90'
    call execute_command_line(syntax_command, wait=.true., &
        exitstat=syntax_exitstat, cmdstat=syntax_status)
    call require(syntax_status == 0 .and. syntax_exitstat == 0, &
        'GNU Fortran rejected the abstract dispatch depth fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'abstract dispatch depth example did not parse')

    root_index = 0
    middle_index = 0
    late_index = 0
    leaf_index = 0
    deep_leaf_index = 0
    local_index = 0
    generic_index = 0
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
        case ('late_t')
            late_index = i
        case ('leaf_t')
            leaf_index = i
        case ('deep_leaf_t')
            deep_leaf_index = i
        case ('local_t')
            local_index = i
        case ('generic_t')
            generic_index = i
        end select
    end do
    call require(root_index > 0 .and. middle_index > 0 .and. &
        late_index > 0 .and. leaf_index > 0 .and. &
        deep_leaf_index > 0 .and. local_index > 0 .and. &
        generic_index > 0, 'abstract dispatch depth types are missing')

    ! Independent source-backed oracle: the abstract contract is implemented
    ! by late_t, inherited once by leaf_t and twice by deep_leaf_t.  local_t
    ! overrides it directly and therefore has depth zero.
    binding = query_type_binding_resolution(result%arena, root_index, 'operate')
    call require(binding%found .and. binding%is_deferred, &
        'root deferred binding was not found')
    call require(size(binding%dispatch_target_type_indices) == 3 .and. &
        size(binding%dispatch_target_inheritance_depth) == 3, &
        'dispatch depth facts are not parallel to concrete targets')
    local_target = find_target(binding%dispatch_target_type_indices, local_index)
    leaf_target = find_target(binding%dispatch_target_type_indices, leaf_index)
    deep_target = find_target(binding%dispatch_target_type_indices, &
        deep_leaf_index)
    call require(local_target > 0 .and. leaf_target > 0 .and. &
        deep_target > 0, 'concrete dispatch targets are incomplete')
    call require(binding%dispatch_target_inheritance_depth(local_target) == 0 .and. &
        .not. binding%dispatch_target_is_inherited(local_target), &
        'local implementation depth is wrong')
    call require(binding%dispatch_target_declaring_type_indices(leaf_target) == &
        late_index .and. binding%dispatch_target_inheritance_depth(leaf_target) == 1 .and. &
        binding%dispatch_target_is_inherited(leaf_target), &
        'one-level inherited implementation depth is wrong')
    call require(binding%dispatch_target_declaring_type_indices(deep_target) == &
        late_index .and. binding%dispatch_target_inheritance_depth(deep_target) == 2 .and. &
        binding%dispatch_target_is_inherited(deep_target), &
        'multi-level inherited implementation depth is wrong')

    ! The call-site query must preserve the same parallel provenance without
    ! making a runtime-flow or ownership claim about the CLASS dummy.
    found_call = .false.
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        call_binding = query_type_bound_call(result%arena, i)
        if (.not. call_binding%found) cycle
        if (trim(call_binding%binding_name) /= 'operate') cycle
        found_call = .true.
        call require(size(call_binding%dispatch_target_inheritance_depth) == 3, &
            'call-site depth facts were not propagated')
        call require(call_binding%dispatch_target_inheritance_depth(local_target) == 0 .and. &
            call_binding%dispatch_target_inheritance_depth(leaf_target) == 1 .and. &
            call_binding%dispatch_target_inheritance_depth(deep_target) == 2, &
            'call-site inheritance depths are wrong')
        exit
    end do
    call require(found_call, 'type-bound operate call was not found')

    ! Generic dispatch remains refusal-only and must not acquire a guessed
    ! target or an inheritance depth.
    binding = query_type_binding_resolution(result%arena, generic_index, 'choose')
    call require(binding%found .and. binding%is_generic .and. &
        size(binding%dispatch_target_type_indices) == 0 .and. &
        size(binding%dispatch_target_inheritance_depth) == 0, &
        'generic refusal exposed a guessed dispatch depth')

    ! A deferred ancestor remains unresolved at its own abstract boundary;
    ! depth facts only describe concrete descendants that already resolved.
    binding = query_type_binding_resolution(result%arena, middle_index, 'operate')
    call require(binding%is_deferred .and. &
        size(binding%dispatch_target_inheritance_depth) == 2, &
        'deferred hierarchy boundary was not preserved')

    print *, 'PASS: abstract dispatch inheritance depth contract'

contains

    include '../common/read_example.inc'

    integer function find_target(indices, wanted) result(found)
        integer, intent(in) :: indices(:), wanted
        integer :: j

        found = 0
        do j = 1, size(indices)
            if (indices(j) == wanted) then
                found = j
                return
            end if
        end do
    end function find_target

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_abstract_dispatch_depth_query
