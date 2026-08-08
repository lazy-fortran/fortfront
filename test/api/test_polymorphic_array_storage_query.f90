program test_polymorphic_array_storage_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, query_storage, storage_query_t, &
        query_component_path, component_path_query_t
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(storage_query_t) :: storage
    type(component_path_query_t) :: path
    character(len=:), allocatable :: source
    integer :: i, abstract_array, concrete_array, owner_component
    integer :: child_component, abstract_paths, concrete_paths

    call read_example('examples/f90/polymorphic_array_storage_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'polymorphic array example was rejected')

    abstract_array = 0
    concrete_array = 0
    owner_component = 0
    child_component = 0
    do i = 1, result%arena%size
        storage = query_storage(result%arena, i)
        if (.not. storage%found) cycle
        select case (trim(storage%name))
        case ('values')
            abstract_array = i
        case ('children')
            concrete_array = i
        case ('owner')
            owner_component = i
        case ('child')
            child_component = i
        end select
    end do
    call require(abstract_array > 0 .and. concrete_array > 0, &
        'array storage declarations were not found')
    call require(owner_component > 0 .and. child_component > 0, &
        'component declarations were not found')

    storage = query_storage(result%arena, abstract_array)
    call require(storage%rank == 1 .and. storage%is_polymorphic .and. &
        storage%is_abstract_type .and. .not. storage%is_concrete_derived, &
        'class(base_t) array facts are incomplete')

    storage = query_storage(result%arena, concrete_array)
    call require(storage%rank == 1 .and. .not. storage%is_polymorphic .and. &
        .not. storage%is_abstract_type .and. storage%is_concrete_derived, &
        'concrete child array facts are incomplete')

    storage = query_storage(result%arena, owner_component)
    call require(storage%is_polymorphic .and. storage%is_abstract_type, &
        'polymorphic abstract component facts are incomplete')
    storage = query_storage(result%arena, child_component)
    call require(.not. storage%is_polymorphic .and. &
        .not. storage%is_abstract_type .and. storage%is_concrete_derived, &
        'concrete component facts are incomplete')

    abstract_paths = 0
    concrete_paths = 0
    do i = 1, result%arena%size
        path = query_component_path(result%arena, i)
        if (.not. path%found) cycle
        if (size(path%component_names) /= 1) cycle
        if (trim(path%component_names(1)) == 'owner') then
            abstract_paths = abstract_paths + 1
            call require(path%is_abstract_type .and. path%is_polymorphic, &
                'abstract component path facts are incomplete')
        else if (trim(path%component_names(1)) == 'child') then
            concrete_paths = concrete_paths + 1
            call require(.not. path%is_abstract_type .and. &
                path%is_concrete_derived, &
                'concrete component path facts are incomplete')
        end if
    end do
    call require(abstract_paths > 0 .and. concrete_paths > 0, &
        'component paths were not exposed')

    print *, 'PASS: polymorphic/abstract array storage query contract'

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

end program test_polymorphic_array_storage_query
