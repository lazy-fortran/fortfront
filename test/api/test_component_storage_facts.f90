program test_component_storage_facts
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, component_access_query_t, query_component_access, &
        component_path_query_t, query_component_path, storage_query_t, &
        query_storage, query_declaration, declaration_query_t, &
        STORAGE_LOCAL, STORAGE_OWNED, STORAGE_POINTER, &
        get_identifier_name
    implicit none

    type :: expected_path_t
        character(len=16) :: first_name = ''
        character(len=16) :: second_name = ''
        integer :: component_count = 0
        integer :: expected_base_rank = 0
        integer :: expected_rank = -1
        integer :: expected_storage = STORAGE_LOCAL
        logical :: expected_array_element = .false.
        logical :: expected_concrete_derived = .false.
        logical :: expected_allocatable = .false.
        logical :: expected_pointer = .false.
        logical :: expected_polymorphic = .false.
        integer :: expected_matches = 1
    end type expected_path_t

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(component_path_query_t) :: path
    type(component_access_query_t) :: component
    type(storage_query_t) :: storage
    type(declaration_query_t) :: declaration
    type(expected_path_t) :: expected(4)
    character(:), allocatable :: source, base_name, error_message
    integer :: i, ordinary_index, pointer_index, alias_index

    call read_example('examples/f90/component_storage_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected component facts example')
    expected(1)%first_name = 'concrete'
    expected(1)%component_count = 1
    expected(1)%expected_rank = 0
    expected(1)%expected_storage = STORAGE_LOCAL
    expected(1)%expected_concrete_derived = .true.
    expected(1)%expected_matches = 2

    expected(2)%first_name = 'values'
    expected(2)%component_count = 1
    expected(2)%expected_rank = 1
    expected(2)%expected_storage = STORAGE_OWNED
    expected(2)%expected_allocatable = .true.
    expected(2)%expected_matches = 2

    expected(3) = expected(1)
    expected(3)%expected_array_element = .true.
    expected(3)%expected_matches = 1

    expected(4)%first_name = 'polymorphic'
    expected(4)%component_count = 1
    expected(4)%expected_rank = 0
    expected(4)%expected_storage = STORAGE_OWNED
    expected(4)%expected_array_element = .true.
    expected(4)%expected_allocatable = .true.
    expected(4)%expected_polymorphic = .true.

    do i = 1, size(expected)
        call require_expected_path(result, expected(i))
    end do

    ordinary_index = 0
    pointer_index = 0
    alias_index = 0
    do i = 1, result%arena%size
        declaration = query_declaration(result%arena, i)
        if (declaration%found) then
            if (trim(declaration%name) == 'ordinary') ordinary_index = i
        end if
        component = query_component_access(result%arena, i)
        if (.not. component%found) cycle
        if (trim(component%component_name) == 'pointer_values') then
            pointer_index = i
        end if
        call get_identifier_name(result%arena, component%base_node_index, &
            base_name, error_message)
        if (trim(base_name) == 'alias') alias_index = i
    end do

    call require(ordinary_index > 0, 'ordinary declaration was not found')
    path = query_component_path(result%arena, ordinary_index)
    call require(.not. path%found, 'ordinary identifier became a component path')
    storage = query_storage(result%arena, ordinary_index)
    call require(storage%found .and. .not. storage%is_component, &
        'ordinary storage fact lost its non-component identity')

    call require(pointer_index > 0, 'pointer component was not found')
    path = query_component_path(result%arena, pointer_index)
    call require(path%found, 'pointer component path was lost')
    storage = query_storage(result%arena, pointer_index)
    call require(storage%found .and. storage%is_component .and. &
        storage%is_pointer .and. .not. storage%is_allocatable .and. &
        storage%storage_class == STORAGE_POINTER, &
        'pointer component was incorrectly classified as owned')
    call require(path%storage_class == STORAGE_POINTER, &
        'pointer path ownership class is missing')

    call require(alias_index > 0, 'associate alias component was not found')
    path = query_component_path(result%arena, alias_index)
    call require(.not. path%found, &
        'associate alias was presented as a resolved component path')
    storage = query_storage(result%arena, alias_index)
    call require(.not. storage%found, &
        'associate alias was presented as resolved storage')

    print *, 'PASS: component storage facts contract'

contains

    include '../common/read_example.inc'

    subroutine require_expected_path(result, expected)
        type(compiler_frontend_result_t), intent(in) :: result
        type(expected_path_t), intent(in) :: expected
        type(component_path_query_t) :: candidate
        type(storage_query_t) :: actual
        integer :: i, matches

        matches = 0
        do i = 1, result%arena%size
            candidate = query_component_path(result%arena, i)
            if (.not. candidate%found) cycle
            if (.not. matches_path(candidate, expected)) cycle
            matches = matches + 1
            actual = query_storage(result%arena, candidate%node_index)
            call require(actual%found, &
                'component storage declaration was not found')
            call require(actual%declaration_index > 0, &
                'component storage declaration identity is missing')
            call require(size(candidate%component_declaration_indices) == &
                expected%component_count, &
                'component declaration path metadata has the wrong length')
            if (size(candidate%component_declaration_indices) > 0) then
                call require(candidate%component_declaration_indices( &
                    size(candidate%component_declaration_indices)) == &
                    actual%declaration_index, &
                    'terminal component declaration identity disagrees')
            end if
            call require(candidate%base_rank == expected%expected_base_rank, &
                'component base rank disagrees with the oracle')
            call require(actual%rank == expected%expected_rank, &
                'component storage rank disagrees with the independent oracle')
            call require(actual%storage_class == expected%expected_storage, &
                'component storage class disagrees with the independent oracle')
            call require(actual%is_array_element .eqv. &
                expected%expected_array_element, &
                'component array-element fact disagrees with the oracle')
            call require(actual%is_concrete_derived .eqv. &
                expected%expected_concrete_derived, &
                'component derived-type fact disagrees with the oracle')
            call require(actual%is_allocatable .eqv. expected%expected_allocatable, &
                'component allocatable fact disagrees with the oracle')
            call require(actual%is_pointer .eqv. expected%expected_pointer, &
                'component pointer fact disagrees with the oracle')
            call require(actual%is_polymorphic .eqv. expected%expected_polymorphic, &
                'component polymorphic fact disagrees with the oracle')
            call require(candidate%rank == expected%expected_rank, &
                'component path rank disagrees with the oracle')
            call require(candidate%storage_class == expected%expected_storage, &
                'component path storage class disagrees with the oracle')
            call require(candidate%is_array_element .eqv. &
                expected%expected_array_element, &
                'component path array-element fact disagrees with the oracle')
            call require(candidate%is_concrete_derived .eqv. &
                expected%expected_concrete_derived, &
                'component path derived-type fact disagrees with the oracle')
            call require(candidate%is_allocatable .eqv. expected%expected_allocatable, &
                'component path allocatable fact disagrees with the oracle')
            call require(candidate%is_polymorphic .eqv. expected%expected_polymorphic, &
                'component path polymorphic fact disagrees with the oracle')
        end do
        call require(matches == expected%expected_matches, &
            'expected component path count is incorrect')
    end subroutine require_expected_path

    logical function matches_path(path, expected)
        type(component_path_query_t), intent(in) :: path
        type(expected_path_t), intent(in) :: expected

        matches_path = size(path%component_names) == expected%component_count
        if (.not. matches_path) return
        if (trim(path%component_names(1)) /= trim(expected%first_name)) then
            matches_path = .false.
            return
        end if
        if (expected%component_count == 2) then
            matches_path = trim(path%component_names(2)) == &
                trim(expected%second_name)
        end if
        if (matches_path) then
            matches_path = path%is_array_element .eqv. &
                expected%expected_array_element
        end if
    end function matches_path

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_component_storage_facts
