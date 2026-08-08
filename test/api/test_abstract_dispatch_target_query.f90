program test_abstract_dispatch_target_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, binding_resolution_query_t, &
        query_type_binding_resolution
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(binding_resolution_query_t) :: binding
    character(len=:), allocatable :: source
    integer :: i, base_index, middle_index, leaf_index

    call read_example('examples/f90/abstract_dispatch_target_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'abstract dispatch example did not parse')

    base_index = 0
    middle_index = 0
    leaf_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        select case (trim(derived%name))
        case ('base_t')
            base_index = i
        case ('middle_t')
            middle_index = i
        case ('leaf_t')
            leaf_index = i
        end select
    end do
    call require(base_index > 0 .and. middle_index > 0 .and. leaf_index > 0, &
        'abstract dispatch type hierarchy was not found')

    binding = query_type_binding_resolution(result%arena, base_index, 'work')
    call require(binding%found, 'base binding was not found')
    call require(size(binding%dispatch_target_type_indices) == 1, &
        'abstract descendant leaked into dispatch targets')
    call require(binding%dispatch_target_type_indices(1) == leaf_index, &
        'concrete leaf dispatch target was not preserved')
    call require(trim(binding%dispatch_target_implementations(1)) == &
        'middle_work', 'inherited concrete implementation was not resolved')

    binding = query_type_binding_resolution(result%arena, middle_index, 'work')
    call require(size(binding%dispatch_target_type_indices) == 1 .and. &
        binding%dispatch_target_type_indices(1) == leaf_index, &
        'abstract intermediate remained a runtime dispatch target')

    print *, 'PASS: abstract dispatch targets are concrete only'

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

end program test_abstract_dispatch_target_query
