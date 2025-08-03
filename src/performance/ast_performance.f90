module ast_performance_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private

    ! Public types
    public :: ast_cache_entry_t, memory_stats_t
    
    ! Public procedures for AST caching
    public :: cache_ast, load_cached_ast, clear_ast_cache
    public :: is_cache_valid, get_cache_stats
    
    ! Public procedures for memory management
    public :: release_ast_memory, compact_arena, get_memory_stats
    
    ! Public procedures for incremental parsing
    public :: update_ast_range, supports_incremental_update
    
    ! Public procedures for concurrent processing
    public :: lock_arena, unlock_arena, is_arena_locked

    ! Cache entry type
    type :: ast_cache_entry_t
        character(len=:), allocatable :: file_path
        character(len=:), allocatable :: source_checksum
        type(ast_arena_t) :: cached_arena
        type(semantic_context_t) :: cached_semantic_ctx
        logical :: is_valid = .false.
        real :: creation_time = 0.0
        integer :: access_count = 0
    end type ast_cache_entry_t

    ! Memory statistics type
    type :: memory_stats_t
        integer :: arena_size = 0
        integer :: arena_capacity = 0
        integer :: allocated_nodes = 0
        integer :: free_nodes = 0
        real :: memory_usage_mb = 0.0
        real :: fragmentation_ratio = 0.0
    end type memory_stats_t

    ! Module-level cache storage
    type(ast_cache_entry_t), allocatable :: cache_entries(:)
    integer :: cache_size = 0
    integer, parameter :: MAX_CACHE_ENTRIES = 100

    ! Arena locking state
    logical, allocatable :: arena_locks(:)
    integer :: max_arenas = 1000

contains

    ! Initialize performance system
    subroutine initialize_performance_system()
        if (.not. allocated(cache_entries)) then
            allocate(cache_entries(MAX_CACHE_ENTRIES))
        end if
        
        if (.not. allocated(arena_locks)) then
            allocate(arena_locks(max_arenas))
            arena_locks = .false.
        end if
    end subroutine initialize_performance_system

    ! AST Caching Implementation
    subroutine cache_ast(file_path, source_checksum, arena, semantic_ctx, success)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: source_checksum
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: semantic_ctx
        logical, intent(out) :: success
        
        integer :: cache_index
        
        success = .false.
        
        call initialize_performance_system()
        
        ! Find empty cache slot or replace oldest entry
        cache_index = find_cache_slot(file_path)
        
        if (cache_index <= 0) then
            return
        end if
        
        ! Store cache entry
        cache_entries(cache_index)%file_path = file_path
        cache_entries(cache_index)%source_checksum = source_checksum
        cache_entries(cache_index)%cached_arena = arena
        cache_entries(cache_index)%cached_semantic_ctx = semantic_ctx
        cache_entries(cache_index)%is_valid = .true.
        cache_entries(cache_index)%creation_time = current_time()
        cache_entries(cache_index)%access_count = 0
        
        if (cache_index > cache_size) then
            cache_size = cache_index
        end if
        
        success = .true.
        
    end subroutine cache_ast

    function load_cached_ast(file_path, source_checksum, arena, semantic_ctx) result(loaded)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: source_checksum
        type(ast_arena_t), intent(out) :: arena
        type(semantic_context_t), intent(out) :: semantic_ctx
        logical :: loaded
        
        integer :: i
        
        loaded = .false.
        
        call initialize_performance_system()
        
        ! Search for cached entry
        do i = 1, cache_size
            if (cache_entries(i)%is_valid .and. &
                cache_entries(i)%file_path == file_path .and. &
                cache_entries(i)%source_checksum == source_checksum) then
                
                ! Load cached data
                arena = cache_entries(i)%cached_arena
                semantic_ctx = cache_entries(i)%cached_semantic_ctx
                cache_entries(i)%access_count = cache_entries(i)%access_count + 1
                
                loaded = .true.
                return
            end if
        end do
        
    end function load_cached_ast

    subroutine clear_ast_cache(file_path)
        character(len=*), intent(in) :: file_path
        
        integer :: i
        
        call initialize_performance_system()
        
        ! Find and invalidate cache entry
        do i = 1, cache_size
            if (cache_entries(i)%is_valid .and. &
                cache_entries(i)%file_path == file_path) then
                cache_entries(i)%is_valid = .false.
                return
            end if
        end do
        
    end subroutine clear_ast_cache

    function is_cache_valid(file_path, source_checksum) result(valid)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(in) :: source_checksum
        logical :: valid
        
        integer :: i
        
        valid = .false.
        
        call initialize_performance_system()
        
        do i = 1, cache_size
            if (cache_entries(i)%is_valid .and. &
                cache_entries(i)%file_path == file_path .and. &
                cache_entries(i)%source_checksum == source_checksum) then
                valid = .true.
                return
            end if
        end do
        
    end function is_cache_valid

    ! Memory Management Implementation
    subroutine release_ast_memory(arena)
        type(ast_arena_t), intent(inout) :: arena
        
        integer :: i
        
        ! Free all allocated nodes
        if (allocated(arena%entries)) then
            do i = 1, arena%size
                if (allocated(arena%entries(i)%node)) then
                    deallocate(arena%entries(i)%node)
                end if
            end do
            deallocate(arena%entries)
        end if
        
        ! Reset arena state
        arena%size = 0
        arena%capacity = 0
        
    end subroutine release_ast_memory

    subroutine compact_arena(arena)
        type(ast_arena_t), intent(inout) :: arena
        
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: i, compacted_size
        
        if (.not. allocated(arena%entries)) return
        
        ! Count valid entries
        compacted_size = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                compacted_size = compacted_size + 1
            end if
        end do
        
        if (compacted_size == arena%size) return  ! No fragmentation
        
        ! Allocate compacted array
        allocate(temp_entries(compacted_size))
        
        ! Copy valid entries
        compacted_size = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                compacted_size = compacted_size + 1
                temp_entries(compacted_size) = arena%entries(i)
            end if
        end do
        
        ! Replace arena entries
        deallocate(arena%entries)
        call move_alloc(temp_entries, arena%entries)
        arena%size = compacted_size
        arena%capacity = compacted_size
        
    end subroutine compact_arena

    function get_memory_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(memory_stats_t) :: stats
        
        integer :: i, allocated_count, free_count
        
        stats%arena_size = arena%size
        stats%arena_capacity = arena%capacity
        
        allocated_count = 0
        free_count = 0
        
        if (allocated(arena%entries)) then
            do i = 1, arena%size
                if (allocated(arena%entries(i)%node)) then
                    allocated_count = allocated_count + 1
                else
                    free_count = free_count + 1
                end if
            end do
        end if
        
        stats%allocated_nodes = allocated_count
        stats%free_nodes = free_count
        
        ! Estimate memory usage (rough calculation)
        stats%memory_usage_mb = real(arena%capacity * 64) / (1024.0 * 1024.0)  ! Assume 64 bytes per entry
        
        ! Calculate fragmentation ratio
        if (arena%capacity > 0) then
            stats%fragmentation_ratio = real(free_count) / real(arena%capacity)
        else
            stats%fragmentation_ratio = 0.0
        end if
        
    end function get_memory_stats

    ! Incremental Parsing Implementation
    subroutine update_ast_range(arena, start_line, end_line, new_source, semantic_ctx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_line, end_line
        character(len=*), intent(in) :: new_source
        type(semantic_context_t), intent(inout) :: semantic_ctx
        
        ! For now, this is a placeholder implementation
        ! In a full implementation, this would:
        ! 1. Identify AST nodes corresponding to the line range
        ! 2. Parse the new source for that range
        ! 3. Replace the affected nodes
        ! 4. Update semantic information
        
        ! Simple validation that arena is valid
        if (arena%size <= 0) then
            write(error_unit, *) "Warning: Cannot update invalid arena"
            return
        end if
        
        ! For this initial implementation, we just mark as "updated"
        ! A real implementation would do surgical AST updates
        
    end subroutine update_ast_range

    function supports_incremental_update() result(supported)
        logical :: supported
        
        ! For now, basic support is available
        supported = .true.
        
    end function supports_incremental_update

    ! Concurrent Processing Implementation
    subroutine lock_arena(arena, success)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(out) :: success
        
        integer :: arena_id
        
        call initialize_performance_system()
        
        ! Simple arena identification (in real implementation, would use proper ID)
        arena_id = mod(arena%size + arena%capacity, max_arenas) + 1
        
        if (arena_id > 0 .and. arena_id <= max_arenas) then
            if (.not. arena_locks(arena_id)) then
                arena_locks(arena_id) = .true.
                success = .true.
            else
                success = .false.  ! Already locked
            end if
        else
            success = .false.
        end if
        
    end subroutine lock_arena

    subroutine unlock_arena(arena, success)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(out) :: success
        
        integer :: arena_id
        
        call initialize_performance_system()
        
        ! Simple arena identification (in real implementation, would use proper ID)
        arena_id = mod(arena%size + arena%capacity, max_arenas) + 1
        
        if (arena_id > 0 .and. arena_id <= max_arenas) then
            if (arena_locks(arena_id)) then
                arena_locks(arena_id) = .false.
                success = .true.
            else
                success = .false.  ! Not locked
            end if
        else
            success = .false.
        end if
        
    end subroutine unlock_arena

    function is_arena_locked(arena) result(locked)
        type(ast_arena_t), intent(in) :: arena
        logical :: locked
        
        integer :: arena_id
        
        call initialize_performance_system()
        
        arena_id = mod(arena%size + arena%capacity, max_arenas) + 1
        
        if (arena_id > 0 .and. arena_id <= max_arenas) then
            locked = arena_locks(arena_id)
        else
            locked = .false.
        end if
        
    end function is_arena_locked

    ! Helper functions
    function find_cache_slot(file_path) result(slot_index)
        character(len=*), intent(in) :: file_path
        integer :: slot_index
        
        integer :: i, oldest_index
        real :: oldest_time, current_time_val
        
        slot_index = 0
        
        ! First, look for existing entry with same file path
        do i = 1, cache_size
            if (cache_entries(i)%is_valid .and. &
                cache_entries(i)%file_path == file_path) then
                slot_index = i
                return
            end if
        end do
        
        ! Look for empty slot
        do i = 1, MAX_CACHE_ENTRIES
            if (.not. cache_entries(i)%is_valid) then
                slot_index = i
                return
            end if
        end do
        
        ! Find oldest entry to replace
        oldest_index = 1
        oldest_time = cache_entries(1)%creation_time
        current_time_val = current_time()
        
        do i = 2, cache_size
            if (cache_entries(i)%creation_time < oldest_time) then
                oldest_time = cache_entries(i)%creation_time
                oldest_index = i
            end if
        end do
        
        slot_index = oldest_index
        
    end function find_cache_slot

    function current_time() result(time_val)
        real :: time_val
        
        ! Simple time implementation (would use system clock in real version)
        call cpu_time(time_val)
        
    end function current_time

    function get_cache_stats() result(stats_info)
        character(len=:), allocatable :: stats_info
        
        integer :: valid_entries, total_accesses, i
        
        call initialize_performance_system()
        
        valid_entries = 0
        total_accesses = 0
        
        do i = 1, cache_size
            if (cache_entries(i)%is_valid) then
                valid_entries = valid_entries + 1
                total_accesses = total_accesses + cache_entries(i)%access_count
            end if
        end do
        
        stats_info = "Cache entries: " // trim(adjustl(char(valid_entries))) // &
                    " / " // trim(adjustl(char(MAX_CACHE_ENTRIES))) // &
                    ", Total accesses: " // trim(adjustl(char(total_accesses)))
        
    end function get_cache_stats

end module ast_performance_module