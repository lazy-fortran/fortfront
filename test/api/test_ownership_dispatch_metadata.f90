program test_ownership_dispatch_metadata
    use fortfront, only: ast_arena_t, compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at
    use fortfront_compiler, only: declaration_query_t, query_declaration, &
        derived_type_query_t, query_derived_type, storage_query_t, query_storage, &
        STORAGE_OWNED, STORAGE_SAVE, STORAGE_COMMON, ownership_event_query_t, &
        query_ownership_events, OWNERSHIP_EVENT_ALLOCATE, &
        OWNERSHIP_EVENT_MOVE_ALLOC, OWNERSHIP_EVENT_NULLIFY, &
        component_path_query_t, query_component_path, &
        binding_resolution_query_t, query_type_binding_resolution, &
        global_reference_query_t, query_active_global_references, ACCESS_WRITE
    use fortfront, only: common_block_query_t, query_common_block
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(:), allocatable :: source
    integer :: i, module_index, base_index, child_index, temporary_index
    integer :: global_index, common_index, event_count, path_count
    integer :: global_refs, common_refs, nullify_count
    type(declaration_query_t) :: declaration
    type(storage_query_t) :: storage
    type(derived_type_query_t) :: derived
    type(ownership_event_query_t), allocatable :: events(:)
    type(component_path_query_t) :: path
    type(binding_resolution_query_t) :: binding
    type(global_reference_query_t), allocatable :: references(:)

    call read_example('examples/f90/ownership_dispatch_metadata.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) call fail('frontend rejected metadata example')

    module_index = 0
    base_index = 0
    child_index = 0
    temporary_index = 0
    global_index = 0
    common_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        select case (trim(get_node_type_at(result%arena, i)))
            case ('module_node')
            module_index = i
            case ('derived_type')
            derived = query_derived_type(result%arena, i)
            if (trim(derived%name) == 'base_t') base_index = i
            if (trim(derived%name) == 'child_t') child_index = i
            case ('declaration')
            declaration = query_declaration(result%arena, i)
            if (trim(declaration%name) == 'temporary') temporary_index = i
            if (trim(declaration%name) == 'global_counter') global_index = i
            if (trim(declaration%name) == 'common_value') common_index = i
        end select
    end do
    call require(module_index > 0, 'module node missing')
    call require(base_index > 0 .and. child_index > 0, 'type nodes missing')
    call require(temporary_index > 0 .and. global_index > 0 .and. &
                 common_index > 0, 'state declarations missing')

    storage = query_storage(result%arena, temporary_index)
    call require(storage%storage_class == STORAGE_OWNED, &
                 'local allocatable is not owned')
    storage = query_storage(result%arena, global_index)
    call require(storage%storage_class == STORAGE_SAVE, &
                 'SAVE state is not classified')
    storage = query_storage(result%arena, common_index)
    call require(storage%storage_class == STORAGE_COMMON, &
                 'COMMON state is not classified')

    events = query_ownership_events(result%arena, module_index)
    event_count = 0
    nullify_count = 0
    do i = 1, size(events)
        if (events(i)%event_kind == OWNERSHIP_EVENT_ALLOCATE .or. &
            events(i)%event_kind == OWNERSHIP_EVENT_MOVE_ALLOC) &
            event_count = event_count + 1
        if (events(i)%event_kind == OWNERSHIP_EVENT_NULLIFY) &
            nullify_count = nullify_count + 1
    end do
    call require(event_count == 2, 'allocation lifetime events incomplete')
    call require(nullify_count == 1, 'NULLIFY lifetime event missing')

    path_count = 0
    do i = 1, result%arena%size
        path = query_component_path(result%arena, i)
        if (.not. path%found) cycle
        if (size(path%component_names) == 1 .and. &
            trim(path%component_names(1)) == 'owned' .and. &
            path%base_node_index > 0) path_count = path_count + 1
    end do
    call require(path_count > 0, 'component path was not reconstructed')

    binding = query_type_binding_resolution(result%arena, base_index, 'run')
    call require(binding%found .and. binding%is_generic, &
                 'generic binding lookup failed')
    call require(size(binding%dispatch_target_type_indices) == 1 .and. &
                 binding%dispatch_target_type_indices(1) == child_index, &
                 'generic dispatch target was not resolved')
    binding = query_type_binding_resolution(result%arena, base_index, 'work')
    call require(binding%found .and. binding%is_deferred .and. binding%pass_arg .and. &
                 trim(binding%pass_name) == 'self', 'deferred PASS metadata failed')
    call require(size(binding%dispatch_target_type_indices) == 1 .and. &
                 binding%dispatch_target_type_indices(1) == child_index, &
                 'dynamic dispatch target was not resolved')

    references = query_active_global_references(result%arena, module_index)
    global_refs = 0
    common_refs = 0
    do i = 1, size(references)
        if (references(i)%is_save_state) then
            global_refs = global_refs + 1
            if (references(i)%access_kind == ACCESS_WRITE) &
                call require(.true., 'write access')
        end if
        if (references(i)%is_common_state) common_refs = common_refs + 1
    end do
    call require(global_refs > 0, 'active SAVE reference missing')
    call require(common_refs > 0, 'active COMMON reference missing')
    print *, 'PASS: ownership and dispatch metadata contract'

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

end program test_ownership_dispatch_metadata
