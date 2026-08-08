program test_abstract_binding_hierarchy_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, binding_hierarchy_query_t, &
        query_type_binding_hierarchy, binding_resolution_query_t, &
        query_type_binding_resolution
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(binding_hierarchy_query_t) :: hierarchy
    type(binding_resolution_query_t) :: resolution
    character(len=:), allocatable :: source
    integer :: i, root_index, middle_index, leaf_index, generic_index
    integer :: target, leaf_target, leaf_implementation_index

    call read_example('examples/f90/abstract_binding_hierarchy_facts.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'abstract hierarchy example did not parse')

    root_index = 0
    middle_index = 0
    leaf_index = 0
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
        case ('leaf_t')
            leaf_index = i
        case ('generic_t')
            generic_index = i
        end select
    end do
    call require(root_index > 0 .and. middle_index > 0 .and. &
        leaf_index > 0 .and. generic_index > 0, &
        'hierarchy types are missing')

    ! The expected values below are an independent oracle: they are facts
    ! defined by the source contract, not values discovered from the AST.
    hierarchy = query_type_binding_hierarchy(result%arena, leaf_index, &
        'operate')
    call require(hierarchy%found .and. hierarchy%is_resolved, &
        'leaf binding was not statically resolved')
    call require(trim(hierarchy%declared_type_name) == 'leaf_t' .and. &
        trim(hierarchy%declaring_type_name) == 'leaf_t' .and. &
        .not. hierarchy%is_inherited, 'leaf declaring/inherited facts wrong')
    call require(trim(hierarchy%implementation) == 'leaf_operate' .and. &
        hierarchy%implementation_node_index > 0, &
        'leaf implementation target was not exposed')
    leaf_implementation_index = hierarchy%implementation_node_index
    call require(trim(hierarchy%pass_name) == 'state' .and. &
        trim(hierarchy%implementation_pass_name) == 'state' .and. &
        hierarchy%implementation_pass_position == 1 .and. &
        trim(hierarchy%implementation_passed_object_type) == 'class(leaf_t)' .and. &
        hierarchy%implementation_signature_resolved, &
        'named PASS implementation signature facts are wrong')
    call require(size(hierarchy%hierarchy) == 3, &
        'multi-level hierarchy length is wrong')
    call require(hierarchy%hierarchy(1)%is_local .and. &
        hierarchy%hierarchy(1)%is_resolved, &
        'leaf local binding facts are wrong')
    call require(hierarchy%hierarchy(2)%is_inherited .and. &
        hierarchy%hierarchy(2)%is_abstract_type .and. &
        hierarchy%hierarchy(2)%is_deferred .and. &
        trim(hierarchy%hierarchy(2)%declaring_type_name) == 'root_t' .and. &
        hierarchy%hierarchy(2)%implementation_node_index == 0 .and. &
        .not. hierarchy%hierarchy(2)%implementation_signature_resolved, &
        'middle deferred inheritance facts are wrong')
    call require(hierarchy%hierarchy(3)%is_local .and. &
        hierarchy%hierarchy(3)%is_abstract_type .and. &
        hierarchy%hierarchy(3)%is_deferred .and. &
        trim(hierarchy%hierarchy(3)%pass_name) == '', &
        'root implicit PASS/deferred facts are wrong')

    hierarchy = query_type_binding_hierarchy(result%arena, middle_index, &
        'operate')
    call require(hierarchy%found .and. hierarchy%is_inherited .and. &
        hierarchy%is_deferred .and. .not. hierarchy%is_resolved .and. &
        trim(hierarchy%declaring_type_name) == 'root_t' .and. &
        hierarchy%implementation_node_index == 0 .and. &
        .not. hierarchy%implementation_signature_resolved, &
        'deferred runtime boundary was not preserved')

    resolution = query_type_binding_resolution(result%arena, root_index, &
        'operate')
    call require(resolution%found .and. resolution%is_deferred .and. &
        size(resolution%dispatch_target_type_indices) == 1, &
        'root deferred dispatch target facts are wrong')
    leaf_target = 0
    do target = 1, size(resolution%dispatch_target_type_indices)
        if (resolution%dispatch_target_type_indices(target) == leaf_index) &
            leaf_target = target
    end do
    call require(leaf_target > 0, 'leaf dispatch target was not found')
    call require(trim(resolution%dispatch_target_implementations(leaf_target)) == &
        'leaf_operate' .and. &
        resolution%dispatch_target_implementation_node_indices(leaf_target) == &
        leaf_implementation_index, &
        'dispatch implementation target identity is wrong')
    call require(trim(resolution%dispatch_target_pass_names(leaf_target)) == &
        'state' .and. resolution%dispatch_target_pass_positions(leaf_target) == 1 .and. &
        trim(resolution%dispatch_target_passed_object_types(leaf_target)) == &
        'class(leaf_t)' .and. &
        resolution%dispatch_target_signature_resolved(leaf_target), &
        'dispatch implementation signature is wrong')

    hierarchy = query_type_binding_hierarchy(result%arena, generic_index, &
        'choose')
    call require(hierarchy%found .and. hierarchy%is_generic .and. &
        hierarchy%is_ambiguous .and. .not. hierarchy%is_resolved .and. &
        len_trim(hierarchy%implementation) == 0 .and. &
        hierarchy%implementation_node_index == 0 .and. &
        .not. hierarchy%implementation_signature_resolved, &
        'ambiguous generic boundary was not preserved')

    print *, 'PASS: abstract hierarchy facts contract'

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

end program test_abstract_binding_hierarchy_facts
