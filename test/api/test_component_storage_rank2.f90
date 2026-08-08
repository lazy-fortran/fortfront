program test_component_storage_rank2
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, component_access_query_t, query_component_access, &
        component_path_query_t, query_component_path, storage_query_t, &
        query_storage, query_declaration, declaration_query_t, &
        STORAGE_LOCAL, STORAGE_OWNED
    implicit none

    type :: expected_path_t
        character(len=16) :: first_name = ''
        character(len=16) :: second_name = ''
        integer :: component_count = 1
        integer :: expected_base_rank = -1
        integer :: expected_rank = -1
        integer :: expected_storage = STORAGE_LOCAL
        logical :: expected_array_element = .false.
        logical :: expected_array_section = .false.
        logical :: expected_concrete_derived = .false.
        logical :: expected_allocatable = .false.
        logical :: expected_polymorphic = .false.
        integer :: expected_matches = 0
    end type expected_path_t

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(component_path_query_t) :: path
    type(component_access_query_t) :: component
    type(storage_query_t) :: storage
    type(declaration_query_t) :: declaration
    type(expected_path_t) :: expected(4)
    character(:), allocatable :: source
    integer :: i, items_index, payload_index, owner_index

    call read_example('examples/f90/component_storage_rank2.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected rank-two component example')

    expected(1)%first_name = 'payload'
    expected(1)%expected_base_rank = 0
    expected(1)%expected_rank = 0
    expected(1)%expected_array_element = .true.
    expected(1)%expected_concrete_derived = .true.
    expected(1)%expected_matches = 2

    expected(2) = expected(1)
    expected(2)%expected_base_rank = 2
    expected(2)%expected_rank = 2
    expected(2)%expected_array_element = .false.
    expected(2)%expected_array_section = .true.
    expected(2)%expected_matches = 1

    expected(3)%first_name = 'owner'
    expected(3)%expected_base_rank = 0
    expected(3)%expected_rank = 0
    expected(3)%expected_storage = STORAGE_OWNED
    expected(3)%expected_array_element = .true.
    expected(3)%expected_allocatable = .true.
    expected(3)%expected_polymorphic = .true.
    expected(3)%expected_matches = 1

    expected(4) = expected(2)
    expected(4)%first_name = 'payload'
    expected(4)%second_name = 'value'
    expected(4)%component_count = 2
    expected(4)%expected_concrete_derived = .false.
    expected(4)%expected_matches = 1

    do i = 1, size(expected)
        call require_expected_path(result, expected(i))
    end do

    items_index = 0
    payload_index = 0
    owner_index = 0
    do i = 1, result%arena%size
        declaration = query_declaration(result%arena, i)
        if (.not. declaration%found) cycle
        if (trim(declaration%name) == 'items') items_index = i
        if (trim(declaration%name) == 'payload') payload_index = i
        if (trim(declaration%name) == 'owner') owner_index = i
    end do
    call require(items_index > 0, 'rank-two array declaration was not found')
    call require(payload_index > 0 .and. owner_index > 0, &
        'component declarations were not found')

    do i = 1, result%arena%size
        component = query_component_access(result%arena, i)
        if (.not. component%found) cycle
        path = query_component_path(result%arena, i)
        if (.not. path%found) cycle
        if (size(path%component_names) /= 1) cycle
        call require(size(path%component_declaration_indices) == 1, &
            'rank-two terminal path declaration identity is missing')
        if (trim(path%component_names(1)) == 'payload') then
            call require(path%component_declaration_indices(1) == payload_index, &
                'payload declaration identity is not stable')
        else if (trim(path%component_names(1)) == 'owner') then
            call require(path%component_declaration_indices(1) == owner_index, &
                'owner declaration identity is not stable')
        end if
    end do

    print *, 'PASS: rank-two component storage facts contract'

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
            if (.not. matches_expected_path(candidate, expected)) cycle
            matches = matches + 1
            actual = query_storage(result%arena, candidate%node_index)
            call require(actual%found, 'rank-two component storage fact missing')
            call require(actual%declaration_index > 0, &
                'rank-two component declaration identity missing')
            call require(candidate%base_rank == expected%expected_base_rank, &
                'rank-two component base rank disagrees with oracle')
            call require(candidate%rank == expected%expected_rank .and. &
                actual%rank == expected%expected_rank, &
                'rank-two terminal rank disagrees with oracle')
            call require(candidate%storage_class == expected%expected_storage .and. &
                actual%storage_class == expected%expected_storage, &
                'rank-two storage class disagrees with oracle')
            call require(candidate%is_array_element .eqv. &
                expected%expected_array_element, &
                'rank-two array-element identity disagrees with oracle')
            call require(candidate%is_array_section .eqv. &
                expected%expected_array_section, &
                'rank-two path array-section identity disagrees with oracle')
            call require(actual%is_array_section .eqv. &
                expected%expected_array_section, &
                'rank-two storage array-section identity disagrees with oracle')
            call require(actual%is_concrete_derived .eqv. &
                expected%expected_concrete_derived, &
                'rank-two derived-type fact disagrees with oracle')
            call require(actual%is_allocatable .eqv. expected%expected_allocatable, &
                'rank-two allocatable fact disagrees with oracle')
            call require(actual%is_polymorphic .eqv. expected%expected_polymorphic, &
                'rank-two polymorphic fact disagrees with oracle')
            call require(size(candidate%component_declaration_indices) == &
                expected%component_count, &
                'rank-two component declaration path has wrong length')
            call require(candidate%component_declaration_indices( &
                expected%component_count) == &
                actual%declaration_index, &
                'rank-two terminal declaration identity disagrees')
        end do
        call require(matches == expected%expected_matches, &
            'rank-two component path count disagrees with oracle')
    end subroutine require_expected_path

    logical function matches_expected_path(path, expected)
        type(component_path_query_t), intent(in) :: path
        type(expected_path_t), intent(in) :: expected

        matches_expected_path = size(path%component_names) == &
            expected%component_count
        if (.not. matches_expected_path) return
        matches_expected_path = trim(path%component_names(1)) == &
            trim(expected%first_name)
        if (.not. matches_expected_path) return
        if (expected%component_count == 2) then
            matches_expected_path = trim(path%component_names(2)) == &
                trim(expected%second_name)
        end if
        if (.not. matches_expected_path) return
        matches_expected_path = path%is_array_element .eqv. &
            expected%expected_array_element
        if (.not. matches_expected_path) return
        matches_expected_path = path%is_array_section .eqv. &
            expected%expected_array_section
    end function matches_expected_path

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_component_storage_rank2
