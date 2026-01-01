module ast_arena_modern
    ! Modern AST arena with generation-based handles and efficient operations
    ! Replaces deprecated ast_arena.f90 with safer architecture
    ! Part of unified arena architecture following KISS principles
    !
    ! This module provides a clean interface by re-exporting components from:
    ! - ast_arena_core: Core arena implementation
    ! - ast_arena_compat: Compatibility layer for old API
    ! - base_arena_t interface: Implemented by ast_arena_core_t

    use ast_base, only: ast_node
    use ast_arena_core, only: ast_arena_core_t, ast_handle_t, ast_node_arena_t, &
                              ast_arena_stats_t, ast_free_result_t, &
                              create_ast_arena_core, destroy_ast_arena_core, &
                              is_valid_ast_handle, null_ast_handle
    use ast_arena_compat, only: ast_arena_compat_t, ast_entry_t, &
                                create_ast_arena_compat
    implicit none

    ! Re-export core types and functions
    public :: ast_arena_t, ast_handle_t, ast_node_arena_t, ast_entry_t
    public :: create_ast_arena, destroy_ast_arena
    public :: store_ast_node, get_ast_node, is_valid_ast_handle, null_ast_handle
    public :: ast_arena_stats_t, ast_free_result_t
    ! Safe, compatibility-hiding helpers (index-based)
    public :: has_node_at, get_node_line, get_node_column
    public :: get_inferred_kind_at, get_inferred_details_at
    public :: free_ast_node, is_node_active, get_free_statistics
    ! Child linking helper for factory functions
    public :: link_children_to_parent

    ! Use compatibility arena directly as the main arena type
    type, extends(ast_arena_compat_t) :: ast_arena_t
        character(len=:), allocatable :: source_text
        integer, allocatable :: source_line_starts(:)
    contains
        ! Compatibility method for old API
        procedure :: clear => clear_ast_arena
        ! Override push to sync size field
        procedure :: push => ast_arena_push_with_size_sync
        ! Safe, index-based helpers
        procedure :: has_node_at
        procedure :: get_node_line
        procedure :: get_node_column
        procedure :: get_inferred_kind_at
        procedure :: get_inferred_details_at
    end type ast_arena_t

contains

    subroutine destroy_compat_entries(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i

        if (.not. allocated(arena%entries)) return

        do i = 1, size(arena%entries)
            if (allocated(arena%entries(i)%node)) deallocate (arena%entries(i)%node)
            if (allocated(arena%entries(i)%node_type)) then
                deallocate (arena%entries(i)%node_type)
            end if
            if (allocated(arena%entries(i)%child_indices)) then
                deallocate (arena%entries(i)%child_indices)
            end if
            arena%entries(i)%parent_index = 0
            arena%entries(i)%depth = 0
            arena%entries(i)%child_count = 0
        end do
    end subroutine destroy_compat_entries

    ! Create modern AST arena with compatibility layer
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: capacity

        ! Set default capacity first - PERFORMANCE FIX: Start small for simple programs
        capacity = 16  ! Minimal starting size, will grow as needed
        if (present(initial_capacity)) capacity = initial_capacity

        ! Create compatibility layer with explicit capacity
        arena%ast_arena_compat_t = create_ast_arena_compat(capacity)

        ! CRITICAL FIX: Manually set capacity field to ensure its available
        ! at the ast_arena_t level. This is needed because the parent
        ! assignment may not properly propagate all base fields.
        arena%capacity = capacity
        arena%size = 0  ! Initialize size from base_arena_t
        arena%generation = 1  ! Initialize generation from base_arena_t
    end function create_ast_arena

    ! Push with size synchronization for backward compatibility
    subroutine ast_arena_push_with_size_sync(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index

        ! Call parent push method
        call this%ast_arena_compat_t%push(node, node_type, parent_index)

        ! Sync size field with compat_size
        this%size = this%compat_size

        ! CRITICAL FIX: Sync capacity field to prevent validation errors
        if (allocated(this%entries)) then
            this%capacity = size(this%entries)
        else
            this%capacity = 0
        end if
    end subroutine ast_arena_push_with_size_sync

    ! Destroy AST arena
    subroutine destroy_ast_arena(arena)
        type(ast_arena_t), intent(inout) :: arena

        if (allocated(arena%source_text)) deallocate (arena%source_text)
        if (allocated(arena%source_line_starts)) deallocate (arena%source_line_starts)
        call destroy_ast_arena_core(arena%ast_arena_compat_t%ast_arena_core_t)

        call destroy_compat_entries(arena)
        if (allocated(arena%entries)) deallocate (arena%entries)
        arena%compat_size = 0
        arena%max_depth = 0
        arena%size = 0
        arena%capacity = 0
        arena%generation = 0
    end subroutine destroy_ast_arena

    ! Free an AST node
    function free_ast_node(arena, handle) result(free_result)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_free_result_t) :: free_result

        ! Use the core implementation through compatibility layer
        free_result = arena%ast_arena_compat_t%free_node(handle)

        ! Sync compat_size with actual node count for statistics
        if (free_result%success) then
            arena%ast_arena_compat_t%compat_size = &
                arena%ast_arena_compat_t%ast_arena_core_t%get_node_count()
        end if
    end function free_ast_node

    ! Check if node is active
    function is_node_active(arena, handle) result(is_active)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        logical :: is_active

        ! Use the core implementation through compatibility layer
        is_active = arena%ast_arena_compat_t%is_active(handle)
    end function is_node_active

    ! Get free statistics
    function get_free_statistics(arena) result(stats)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_arena_stats_t) :: stats

        ! Use the core implementation's free stats directly
        stats = arena%ast_arena_compat_t%ast_arena_core_t%get_free_stats()
    end function get_free_statistics

    ! Store an AST node
    function store_ast_node(arena, node) result(ast_handle)
        use ast_arena_core, only: core_store => store_ast_node
        type(ast_arena_t), intent(inout) :: arena
        type(ast_node_arena_t), intent(in) :: node
        type(ast_handle_t) :: ast_handle

        ! Delegate to core function through inheritance casting
        ast_handle = core_store(arena%ast_arena_compat_t%ast_arena_core_t, node)

        ! Sync compat_size with actual node count for statistics
        if (is_valid_ast_handle(ast_handle)) then
            arena%ast_arena_compat_t%compat_size = &
                arena%ast_arena_compat_t%ast_arena_core_t%get_node_count()
        end if
    end function store_ast_node

    ! Get an AST node
    function get_ast_node(arena, handle) result(arena_node)
        use ast_arena_core, only: core_get => get_ast_node
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t) :: arena_node

        ! Delegate to core function through inheritance casting
        arena_node = core_get(arena%ast_arena_compat_t%ast_arena_core_t, handle)
    end function get_ast_node

    ! Clear/reset arena (compatibility wrapper)
    subroutine clear_ast_arena(this)
        class(ast_arena_t), intent(inout) :: this

        ! Delegate to reset method in core
        call this%ast_arena_compat_t%reset()

        ! Sync compat_size with actual node count (should be 0 after reset)
        this%ast_arena_compat_t%compat_size = &
            this%ast_arena_compat_t%ast_arena_core_t%get_node_count()

        if (allocated(this%source_text)) deallocate (this%source_text)
        if (allocated(this%source_line_starts)) deallocate (this%source_line_starts)
    end subroutine clear_ast_arena

    ! =============================
    ! Safe, index-based introspection
    ! These helpers encapsulate compat internals (entries/compat_size)
    ! and provide read-only accessors commonly needed by clients.
    ! =============================

    pure logical function has_node_at(this, index) result(has)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        has = .false.
        if (index <= 0) return
        if (.not. allocated(this%entries)) return
        if (index > size(this%entries)) return
        has = allocated(this%entries(index)%node)
    end function has_node_at

    pure integer function get_node_line(this, index) result(line)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        line = 0
        if (.not. this%has_node_at(index)) return
        line = this%entries(index)%node%line
    end function get_node_line

    pure integer function get_node_column(this, index) result(column)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        column = 0
        if (.not. this%has_node_at(index)) return
        column = this%entries(index)%node%column
    end function get_node_column

    pure integer function get_inferred_kind_at(this, index) result(kind)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        kind = 0
        if (.not. this%has_node_at(index)) return
        if (.not. this%entries(index)%node%inferred_type%kind > 0) return
        kind = this%entries(index)%node%inferred_type%kind
    end function get_inferred_kind_at

    subroutine get_inferred_details_at(this, index, kind, type_size, &
                                       is_allocatable, is_pointer, found)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer, intent(out) :: kind, type_size
        logical, intent(out) :: is_allocatable, is_pointer, found

        kind = 0
        type_size = 0
        is_allocatable = .false.
        is_pointer = .false.
        found = .false.

        if (.not. this%has_node_at(index)) return
        if (.not. this%entries(index)%node%inferred_type%kind > 0) return

        associate (t => this%entries(index)%node%inferred_type)
            kind = t%kind
            type_size = t%size
            is_allocatable = t%alloc_info%is_allocatable
            is_pointer = t%alloc_info%is_pointer
            found = .true.
        end associate
    end subroutine get_inferred_details_at

    ! Link an array of child indices to a parent node
    ! This establishes parent-child relationships in the arena for AST traversal
    ! If a child already has a different parent, it is removed from that parent first
    subroutine link_children_to_parent(arena, parent_index, child_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index
        integer, intent(in) :: child_indices(:)

        integer :: i, child_index
        integer :: valid_size
        integer :: current_parent

        if (.not. allocated(arena%entries)) return

        valid_size = arena%compat_size
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%node)) return

        do i = 1, size(child_indices)
            child_index = child_indices(i)
            if (child_index <= 0 .or. child_index > valid_size) cycle
            if (.not. allocated(arena%entries(child_index)%node)) cycle
            if (child_index == parent_index) cycle

            current_parent = arena%entries(child_index)%parent_index
            if (current_parent > 0 .and. current_parent /= parent_index) then
                call remove_child_from_parent(arena, current_parent, child_index)
            end if

            arena%entries(child_index)%parent_index = parent_index
            arena%entries(child_index)%depth = arena%entries(parent_index)%depth + 1
            if (arena%entries(child_index)%depth > arena%max_depth) then
                arena%max_depth = arena%entries(child_index)%depth
            end if

            if (.not. is_child_present(arena, parent_index, child_index)) then
                call append_child_index(arena, parent_index, child_index)
            end if
        end do
    end subroutine link_children_to_parent

    subroutine append_child_index(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index

        integer, allocatable :: tmp(:)
        integer :: count, new_cap

        if (.not. allocated(arena%entries(parent_index)%child_indices)) then
            new_cap = 4
            allocate (arena%entries(parent_index)%child_indices(new_cap))
            arena%entries(parent_index)%child_count = 1
            arena%entries(parent_index)%child_indices(1) = child_index
            return
        end if

        count = arena%entries(parent_index)%child_count
        if (count < 0) count = 0
        if (count >= size(arena%entries(parent_index)%child_indices)) then
            new_cap = max(2 * &
                          size(arena%entries(parent_index)%child_indices), count + 1)
            allocate (tmp(new_cap))
            if (count > 0) then
                tmp(1:count) = arena%entries(parent_index)%child_indices(1:count)
            end if
            call move_alloc(tmp, arena%entries(parent_index)%child_indices)
        end if

        arena%entries(parent_index)%child_count = count + 1
        arena%entries(parent_index)%child_indices(count + 1) = child_index
    end subroutine append_child_index

    ! Check if child is already present in parent's children list
    pure logical function is_child_present(arena, parent_index, child_index) &
        result(present)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: parent_index, child_index
        integer :: i, count, arr_size, valid_size

        present = .false.
        ! Bounds check for parent_index to prevent out-of-bounds access
        if (.not. allocated(arena%entries)) return
        ! Use compat_size for logical size validation
        valid_size = arena%compat_size
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%child_indices)) return
        count = arena%entries(parent_index)%child_count
        if (count <= 0) return
        arr_size = size(arena%entries(parent_index)%child_indices)
        if (count > arr_size) count = arr_size
        do i = 1, count
            if (arena%entries(parent_index)%child_indices(i) == child_index) then
                present = .true.
                return
            end if
        end do
    end function is_child_present

    ! Remove a child from a parent's children list
    subroutine remove_child_from_parent(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index
        integer :: i, j, old_count, valid_size

        ! Bounds check: ensure parent_index is valid
        if (.not. allocated(arena%entries)) return
        ! Use compat_size for logical size validation
        valid_size = arena%compat_size
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%child_indices)) return
        old_count = arena%entries(parent_index)%child_count
        if (old_count == 0) return
        ! Ensure old_count does not exceed array bounds
        if (old_count > size(arena%entries(parent_index)%child_indices)) then
            old_count = size(arena%entries(parent_index)%child_indices)
        end if

        j = 0
        do i = 1, old_count
            if (arena%entries(parent_index)%child_indices(i) /= child_index) then
                j = j + 1
                arena%entries(parent_index)%child_indices(j) = &
                    arena%entries(parent_index)%child_indices(i)
            end if
        end do
        arena%entries(parent_index)%child_count = j
    end subroutine remove_child_from_parent

end module ast_arena_modern
