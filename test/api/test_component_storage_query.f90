program test_component_storage_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, component_access_query_t, query_component_access, &
        storage_query_t, query_storage, STORAGE_OWNED
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(component_access_query_t) :: component
    type(storage_query_t) :: storage
    character(:), allocatable :: source
    integer :: i, owner_access, payload_access

    call read_example('examples/f90/component_storage_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected component storage example')

    owner_access = 0
    payload_access = 0
    do i = 1, result%arena%size
        component = query_component_access(result%arena, i)
        if (.not. component%found) cycle
        if (trim(component%component_name) == 'owner') owner_access = i
        if (trim(component%component_name) == 'payload') payload_access = i
    end do
    call require(owner_access > 0 .and. payload_access > 0, &
        'component access nodes missing')

    storage = query_storage(result%arena, owner_access)
    call require(storage%found .and. storage%node_index == owner_access .and. &
        trim(storage%name) == 'owner' .and. &
        trim(storage%type_name) == 'class(base_t)' .and. &
        storage%storage_class == STORAGE_OWNED .and. storage%is_allocatable .and. &
        storage%is_polymorphic .and. .not. storage%is_unlimited_polymorphic, &
        'class(base_t) component storage facts are incorrect')

    storage = query_storage(result%arena, payload_access)
    call require(storage%found .and. storage%node_index == payload_access .and. &
        trim(storage%name) == 'payload' .and. &
        trim(storage%type_name) == 'class(*)' .and. &
        storage%storage_class == STORAGE_OWNED .and. storage%is_allocatable .and. &
        storage%is_polymorphic .and. storage%is_unlimited_polymorphic, &
        'class(*) component storage facts are incorrect')

    print *, 'PASS: component storage query contract'

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

end program test_component_storage_query
