program test_allocation_ownership_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, storage_query_t, query_storage, &
        STORAGE_LOCAL, STORAGE_OWNED, STORAGE_POINTER, &
        ownership_event_query_t, query_ownership_events, &
        OWNERSHIP_EVENT_ALLOCATE, component_path_query_t, query_component_path
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(ownership_event_query_t), allocatable :: events(:)
    type(component_path_query_t) :: path
    type(storage_query_t) :: storage
    character(:), allocatable :: source
    integer :: i, module_index, allocate_count, mold_count, source_count
    character(len=6) :: expected_values(1)
    logical :: found_plain, found_link

    call read_example('examples/f90/allocation_ownership_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'frontend rejected allocation ownership example')

    module_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'module_node') then
            module_index = i
            exit
        end if
    end do
    call require(module_index > 0, 'module node missing')
    expected_values(1) = 'values'

    events = query_ownership_events(result%arena, module_index)
    allocate_count = 0
    mold_count = 0
    source_count = 0
    do i = 1, size(events)
        if (events(i)%event_kind /= OWNERSHIP_EVENT_ALLOCATE) cycle
        allocate_count = allocate_count + 1
        call require(size(events(i)%object_indices) == 1, &
            'allocation object fact is incomplete')
        path = query_component_path(result%arena, events(i)%object_indices(1))
        call require(path%found, 'allocation component path is missing')
        storage = query_storage(result%arena, events(i)%object_indices(1))
        call require(storage%found .and. storage%is_allocatable .and. &
            storage%storage_class == STORAGE_OWNED, &
            'allocation target storage fact is incomplete')

        if (events(i)%mold_expr_index > 0) then
            mold_count = mold_count + 1
            call require(events(i)%source_expr_index == 0, &
                'MOLD= event also reports SOURCE=')
            call require_path(result, events(i)%mold_expr_index, expected_values, &
                'MOLD= component path is incomplete')
        else if (events(i)%source_expr_index > 0) then
            source_count = source_count + 1
            call require_path(result, events(i)%source_expr_index, expected_values, &
                'SOURCE= component path is incomplete')
        else
            call require(.false., 'allocation SOURCE=/MOLD= fact is missing')
        end if
    end do
    call require(allocate_count == 2, 'allocation events are incomplete')
    call require(mold_count == 1 .and. source_count == 1, &
        'SOURCE=/MOLD= ownership facts are incomplete')

    found_plain = .false.
    found_link = .false.
    do i = 1, result%arena%size
        path = query_component_path(result%arena, i)
        if (.not. path%found) cycle
        if (size(path%component_names) /= 2) cycle
        if (trim(path%component_names(1)) /= 'nested') cycle
        storage = query_storage(result%arena, i)
        if (.not. storage%found) cycle
        if (trim(path%component_names(2)) == 'plain') then
            found_plain = .true.
            call require(storage%storage_class == STORAGE_LOCAL .and. &
                .not. storage%is_allocatable .and. .not. storage%is_pointer, &
                'ordinary component was classified as owning')
        else if (trim(path%component_names(2)) == 'link') then
            found_link = .true.
            call require(storage%storage_class == STORAGE_POINTER .and. &
                storage%is_pointer .and. .not. storage%is_allocatable, &
                'pointer component was classified as owning')
        end if
    end do
    call require(found_plain .and. found_link, &
        'negative ordinary/non-owning component facts are missing')

    print *, 'PASS: allocation ownership query contract'

contains

    include '../common/read_example.inc'

    subroutine require_path(result, node_index, expected_names, message)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected_names(:)
        character(len=*), intent(in) :: message
        type(component_path_query_t) :: actual_path
        type(storage_query_t) :: actual_storage
        integer :: j

        actual_path = query_component_path(result%arena, node_index)
        call require(actual_path%found, message)
        call require(size(actual_path%component_names) == size(expected_names), message)
        do j = 1, size(expected_names)
            call require(trim(actual_path%component_names(j)) == &
                trim(expected_names(j)), message)
        end do
        actual_storage = query_storage(result%arena, node_index)
        call require(actual_storage%found, message)
    end subroutine require_path

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_allocation_ownership_query
