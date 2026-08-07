program test_binding_hierarchy_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, binding_hierarchy_query_t, &
        query_type_binding_hierarchy
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(binding_hierarchy_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, base_index, intermediate_index, concrete_index
    integer :: ambiguous_index

    base_index = 0
    intermediate_index = 0
    concrete_index = 0
    ambiguous_index = 0

    call read_example('examples/f90/binding_hierarchy_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'binding hierarchy example did not parse')

    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        select case (trim(derived%name))
        case ('base_t')
            base_index = i
        case ('intermediate_t')
            intermediate_index = i
        case ('concrete_t')
            concrete_index = i
        case ('ambiguous_t')
            ambiguous_index = i
        end select
    end do

    call require(base_index > 0, 'base type was not found')
    call require(intermediate_index > 0, 'intermediate type was not found')
    call require(concrete_index > 0, 'concrete type was not found')
    call require(ambiguous_index > 0, 'ambiguous type was not found')

    query = query_type_binding_hierarchy(result%arena, concrete_index, 'run')
    call require(query%found, 'concrete binding was not found')
    call require(trim(query%declared_type_name) == 'concrete_t', &
        'declared type was not reported')
    call require(trim(query%declaring_type_name) == 'concrete_t', &
        'local declaring type was not reported')
    call require(query%is_resolved, 'concrete implementation was not resolved')
    call require(trim(query%implementation) == 'concrete_run', &
        'concrete implementation name is wrong')
    call require(query%pass_arg, 'PASS binding metadata was lost')
    call require(trim(query%pass_name) == 'self', 'PASS name was lost')
    call require(size(query%parent_type_names) == 2, 'parent chain length is wrong')
    call require(trim(query%parent_type_names(1)) == 'intermediate_t', &
        'first parent is wrong')
    call require(trim(query%parent_type_names(2)) == 'base_t', &
        'root parent is wrong')
    call require(size(query%hierarchy) == 3, 'hierarchy entry count is wrong')
    call require(query%hierarchy(1)%is_local, 'concrete binding is not local')
    call require(query%hierarchy(2)%is_inherited, &
        'intermediate inheritance was not reported')
    call require(query%hierarchy(2)%is_abstract_type, &
        'intermediate ABSTRACT status was not reported')

    query = query_type_binding_hierarchy(result%arena, concrete_index, &
        'inherited')
    call require(query%found, 'inherited binding was not found')
    call require(query%is_inherited, 'inherited-only binding was marked local')
    call require(.not. query%hierarchy(1)%is_local, &
        'inherited-only binding has a false local flag')
    call require(query%is_resolved, 'inherited concrete implementation was lost')
    call require(trim(query%implementation) == 'base_inherited', &
        'inherited implementation name is wrong')
    call require(.not. query%pass_arg, 'NOPASS metadata was lost')

    query = query_type_binding_hierarchy(result%arena, intermediate_index, 'run')
    call require(query%found, 'deferred inherited binding was not found')
    call require(query%is_inherited, 'deferred binding was not marked inherited')
    call require(query%is_deferred, 'DEFERRED status was not reported')
    call require(query%is_abstract_type, 'ABSTRACT status was not reported')
    call require(.not. query%is_resolved, 'deferred binding was resolved')
    call require(len_trim(query%implementation) == 0, &
        'deferred binding received an implementation guess')

    query = query_type_binding_hierarchy(result%arena, ambiguous_index, &
        'ambiguous')
    call require(query%found, 'ambiguous binding was not found')
    call require(query%is_ambiguous, 'ambiguous generic was not marked ambiguous')
    call require(.not. query%is_resolved, 'ambiguous generic was resolved')
    call require(len_trim(query%implementation) == 0, &
        'ambiguous generic received an implementation guess')

    query = query_type_binding_hierarchy(result%arena, concrete_index, 'missing')
    call require(.not. query%found, 'missing binding was invented')
    call require(query%is_unresolved, 'missing binding was not marked unresolved')

    print *, 'PASS: binding hierarchy query contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) call fail(message)
    end subroutine require

    subroutine fail(message)
        character(len=*), intent(in) :: message
        print *, 'FAIL: ', trim(message)
        error stop 1
    end subroutine fail

end program test_binding_hierarchy_query
