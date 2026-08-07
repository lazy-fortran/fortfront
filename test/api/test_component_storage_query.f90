program test_component_storage_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, component_access_query_t, query_component_access, &
        component_path_query_t, query_component_path, storage_query_t, &
        query_storage, STORAGE_OWNED
    implicit none

    type :: expected_fact_t
        character(len=16) :: first_name = ''
        character(len=16) :: second_name = ''
        integer :: component_count = 0
        character(len=32) :: type_name = ''
        integer :: storage_class = 0
        logical :: is_allocatable = .false.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
    end type expected_fact_t

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(component_access_query_t) :: component
    type(component_path_query_t) :: path
    type(storage_query_t) :: storage
    type(expected_fact_t) :: expected(4)
    character(:), allocatable :: source
    integer :: i, component_count, element_index

    call read_example('examples/f90/component_storage_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected component storage example')

    expected(1)%first_name = 'owner'
    expected(1)%component_count = 1
    expected(1)%type_name = 'class(base_t)'
    expected(1)%storage_class = STORAGE_OWNED
    expected(1)%is_allocatable = .true.
    expected(1)%is_polymorphic = .true.

    expected(2) = expected(1)
    expected(2)%first_name = 'payload'
    expected(2)%type_name = 'class(*)'
    expected(2)%is_unlimited_polymorphic = .true.

    expected(3) = expected(1)
    expected(3)%first_name = 'nested'
    expected(3)%second_name = 'owner'
    expected(3)%component_count = 2

    expected(4) = expected(2)
    expected(4)%first_name = 'nested'
    expected(4)%second_name = 'payload'
    expected(4)%component_count = 2

    do i = 1, size(expected)
        call require_expected_fact(result, expected(i))
    end do

    component_count = 0
    do i = 1, result%arena%size
        component = query_component_access(result%arena, i)
        if (.not. component%found) cycle
        component_count = component_count + 1
    end do
    call require(component_count == 6, 'component access nodes missing')

    do i = 1, result%arena%size
        path = query_component_path(result%arena, i)
        if (.not. path%found) cycle
        if (size(path%component_names) /= 1) cycle
        if (trim(path%component_names(1)) /= 'owner' .and. &
            trim(path%component_names(1)) /= 'payload') cycle
        element_index = path%base_node_index
        path = query_component_path(result%arena, path%base_node_index)
        call require(.not. path%found, &
            'array element was reported as a component path')
        storage = query_storage(result%arena, element_index)
        call require(.not. storage%found, &
            'array element was reported as storage')
        exit
    end do

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

    subroutine require_expected_fact(result, expected)
        type(compiler_frontend_result_t), intent(in) :: result
        type(expected_fact_t), intent(in) :: expected
        type(component_path_query_t) :: candidate
        type(storage_query_t) :: actual
        character(len=16) :: expected_name
        integer :: i, matches

        matches = 0
        do i = 1, result%arena%size
            candidate = query_component_path(result%arena, i)
            if (.not. candidate%found) cycle
            if (.not. matches_expected_path(candidate, expected)) cycle
            matches = matches + 1
            actual = query_storage(result%arena, candidate%node_index)
            call require(actual%found .and. actual%node_index == candidate%node_index, &
                'expected component storage fact was not found')
            expected_name = expected%first_name
            if (expected%component_count == 2) expected_name = expected%second_name
            call require(trim(actual%name) == trim(expected_name), &
                'component storage name is incorrect')
            call require(trim(actual%type_name) == trim(expected%type_name) .and. &
                actual%storage_class == expected%storage_class .and. &
                actual%is_allocatable .eqv. expected%is_allocatable .and. &
                actual%is_polymorphic .eqv. expected%is_polymorphic .and. &
                actual%is_unlimited_polymorphic .eqv. &
                expected%is_unlimited_polymorphic, &
                'component storage facts are incorrect')
        end do
        call require(matches == 1, 'expected component path count is incorrect')
    end subroutine require_expected_fact

    logical function matches_expected_path(path, expected)
        type(component_path_query_t), intent(in) :: path
        type(expected_fact_t), intent(in) :: expected

        matches_expected_path = size(path%component_names) == expected%component_count
        if (.not. matches_expected_path) return
        if (trim(path%component_names(1)) /= trim(expected%first_name)) then
            matches_expected_path = .false.
            return
        end if
        if (expected%component_count == 2) then
            matches_expected_path = trim(path%component_names(2)) == &
                trim(expected%second_name)
        end if
    end function matches_expected_path

end program test_component_storage_query
