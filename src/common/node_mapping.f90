module node_mapping
    ! Node Mapping System for CST/AST Bidirectional Navigation
    ! ========================================================
    ! Provides fast O(log n) bidirectional lookups between CST and AST nodes
    ! using UID-based mapping. Essential for external tooling that needs to
    ! navigate between source-preserving CST and semantically-clean AST.
    !
    ! Features:
    ! - UID-based bidirectional mapping
    ! - O(log n) lookup performance via binary search
    ! - Memory-efficient storage
    ! - Support for tool integration
    
    use, intrinsic :: iso_fortran_env, only: int64
    use uid_generator, only: uid_t, uid_equal
    use error_handling, only: result_t, success_result, create_error_result, &
                             ERROR_VALIDATION, ERROR_INTERNAL
    implicit none
    private
    
    public :: node_mapping_t, mapping_entry_t
    public :: create_node_mapping
    public :: lookup_result_t
    public :: node_mapping_add, node_mapping_cst_to_ast, node_mapping_ast_to_cst
    public :: node_mapping_clear, node_mapping_is_empty, node_mapping_get_size
    
    ! Single mapping entry linking CST and AST nodes via UIDs
    type :: mapping_entry_t
        type(uid_t) :: cst_uid          ! CST node UID
        type(uid_t) :: ast_uid          ! Corresponding AST node UID
        integer :: cst_arena_index      ! CST arena index for fast access
        integer :: ast_arena_index      ! AST arena index for fast access
        integer :: mapping_generation   ! Generation for invalidation
    contains
        procedure :: assign => mapping_entry_assign
        generic :: assignment(=) => assign
    end type mapping_entry_t
    
    ! Result type for lookup operations
    type :: lookup_result_t
        type(result_t) :: result        ! Success/failure status
        type(uid_t) :: found_uid        ! Found UID (if successful)
        integer :: arena_index = 0     ! Arena index (if successful)
        logical :: found = .false.     ! Whether mapping was found
    end type lookup_result_t
    
    ! High-performance bidirectional node mapping system
    type :: node_mapping_t
        type(mapping_entry_t), allocatable :: entries(:)  ! Sorted by CST UID
        integer :: size = 0                               ! Current number of entries
        integer :: capacity = 0                           ! Array capacity
        integer :: current_generation = 1                 ! Current mapping generation
        logical :: is_sorted = .true.                     ! Whether entries are sorted
    contains
        procedure :: add_mapping => node_mapping_add
        procedure :: cst_to_ast => node_mapping_cst_to_ast
        procedure :: ast_to_cst => node_mapping_ast_to_cst
        procedure :: remove_mapping => node_mapping_remove
        procedure :: clear => node_mapping_clear
        procedure :: get_size => node_mapping_get_size
        procedure :: is_empty => node_mapping_is_empty
        procedure :: sort_entries => node_mapping_sort
        procedure :: find_by_cst_uid => node_mapping_find_cst_uid
        procedure :: find_by_ast_uid => node_mapping_find_ast_uid
        procedure :: validate_mapping => node_mapping_validate
        procedure :: get_stats => node_mapping_get_stats
        procedure :: resize => node_mapping_resize
    end type node_mapping_t
    
    ! Statistics for performance monitoring
    type :: mapping_stats_t
        integer :: total_mappings = 0
        integer :: cst_to_ast_lookups = 0
        integer :: ast_to_cst_lookups = 0
        integer :: average_lookup_time_ns = 0
        integer :: memory_usage_bytes = 0
    end type mapping_stats_t
    
contains
    
    ! Create new node mapping system
    function create_node_mapping(initial_capacity) result(mapping)
        integer, intent(in), optional :: initial_capacity
        type(node_mapping_t) :: mapping
        
        integer :: capacity
        
        capacity = 256  ! Default capacity
        if (present(initial_capacity)) capacity = max(initial_capacity, 16)
        
        mapping%capacity = capacity
        mapping%size = 0
        mapping%current_generation = 1
        mapping%is_sorted = .true.
        
        allocate(mapping%entries(capacity))
    end function create_node_mapping
    
    ! Validate mapping input parameters
    function validate_mapping_inputs(cst_uid, ast_uid, cst_index, ast_index) result(res)
        type(uid_t), intent(in) :: cst_uid
        type(uid_t), intent(in) :: ast_uid
        integer, intent(in) :: cst_index
        integer, intent(in) :: ast_index
        type(result_t) :: res
        
        if (.not. cst_uid%is_valid() .or. .not. ast_uid%is_valid()) then
            res = create_error_result( &
                "Invalid UIDs provided for mapping", &
                ERROR_VALIDATION, &
                component="node_mapping", &
                context="add_mapping", &
                suggestion="Ensure both CST and AST UIDs are valid" &
            )
            return
        end if
        
        if (cst_index <= 0 .or. ast_index <= 0) then
            res = create_error_result( &
                "Invalid arena indices provided for mapping", &
                ERROR_VALIDATION, &
                component="node_mapping", &
                context="add_mapping", &
                suggestion="Arena indices must be positive" &
            )
            return
        end if
        
        res = success_result()
    end function validate_mapping_inputs
    
    ! Create mapping entry from validated inputs
    function create_mapping_entry(cst_uid, ast_uid, cst_index, ast_index, generation) result(entry)
        type(uid_t), intent(in) :: cst_uid
        type(uid_t), intent(in) :: ast_uid
        integer, intent(in) :: cst_index
        integer, intent(in) :: ast_index
        integer, intent(in) :: generation
        type(mapping_entry_t) :: entry
        
        entry%cst_uid = cst_uid
        entry%ast_uid = ast_uid
        entry%cst_arena_index = cst_index
        entry%ast_arena_index = ast_index
        entry%mapping_generation = generation
    end function create_mapping_entry
    
    ! Add bidirectional mapping between CST and AST nodes
    function node_mapping_add(this, cst_uid, ast_uid, cst_index, ast_index) result(res)
        class(node_mapping_t), intent(inout) :: this
        type(uid_t), intent(in) :: cst_uid
        type(uid_t), intent(in) :: ast_uid
        integer, intent(in) :: cst_index
        integer, intent(in) :: ast_index
        type(result_t) :: res
        
        type(mapping_entry_t) :: entry
        
        ! Validate inputs
        res = validate_mapping_inputs(cst_uid, ast_uid, cst_index, ast_index)
        if (res%is_failure()) return
        
        ! Resize if necessary
        if (this%size >= this%capacity) then
            call this%resize(this%capacity * 2)
        end if
        
        ! Create new mapping entry
        entry = create_mapping_entry(cst_uid, ast_uid, cst_index, ast_index, this%current_generation)
        
        ! Add to array
        this%size = this%size + 1
        this%entries(this%size) = entry
        this%is_sorted = .false.  ! Mark as unsorted
        
        res = success_result()
    end function node_mapping_add
    
    ! Look up AST UID and index from CST UID
    function node_mapping_cst_to_ast(this, cst_uid) result(lookup_res)
        class(node_mapping_t), intent(inout) :: this
        type(uid_t), intent(in) :: cst_uid
        type(lookup_result_t) :: lookup_res
        
        integer :: index
        
        ! Initialize result
        lookup_res%result = success_result()
        lookup_res%found = .false.
        lookup_res%arena_index = 0
        
        ! Validate input
        if (.not. cst_uid%is_valid()) then
            lookup_res%result = create_error_result( &
                "Invalid CST UID provided for lookup", &
                ERROR_VALIDATION, &
                component="node_mapping", &
                context="cst_to_ast", &
                suggestion="Ensure CST UID is valid" &
            )
            return
        end if
        
        ! Sort entries if needed for binary search
        if (.not. this%is_sorted) call this%sort_entries()
        
        ! Binary search by CST UID
        index = this%find_by_cst_uid(cst_uid)
        if (index > 0) then
            lookup_res%found = .true.
            lookup_res%found_uid = this%entries(index)%ast_uid
            lookup_res%arena_index = this%entries(index)%ast_arena_index
        end if
    end function node_mapping_cst_to_ast
    
    ! Look up CST UID and index from AST UID
    function node_mapping_ast_to_cst(this, ast_uid) result(lookup_res)
        class(node_mapping_t), intent(inout) :: this
        type(uid_t), intent(in) :: ast_uid
        type(lookup_result_t) :: lookup_res
        
        integer :: index
        
        ! Initialize result
        lookup_res%result = success_result()
        lookup_res%found = .false.
        lookup_res%arena_index = 0
        
        ! Validate input
        if (.not. ast_uid%is_valid()) then
            lookup_res%result = create_error_result( &
                "Invalid AST UID provided for lookup", &
                ERROR_VALIDATION, &
                component="node_mapping", &
                context="ast_to_cst", &
                suggestion="Ensure AST UID is valid" &
            )
            return
        end if
        
        ! Linear search by AST UID (could optimize with secondary index)
        index = this%find_by_ast_uid(ast_uid)
        if (index > 0) then
            lookup_res%found = .true.
            lookup_res%found_uid = this%entries(index)%cst_uid
            lookup_res%arena_index = this%entries(index)%cst_arena_index
        end if
    end function node_mapping_ast_to_cst
    
    ! Find entry index by CST UID using binary search
    function node_mapping_find_cst_uid(this, cst_uid) result(index)
        class(node_mapping_t), intent(in) :: this
        type(uid_t), intent(in) :: cst_uid
        integer :: index
        
        integer :: left, right, mid
        
        index = 0  ! Not found
        if (this%size == 0) return
        
        left = 1
        right = this%size
        
        ! Binary search (assumes entries are sorted by CST UID)
        do while (left <= right)
            mid = left + (right - left) / 2
            
            if (uid_equal(this%entries(mid)%cst_uid, cst_uid)) then
                index = mid
                return
            else if (this%entries(mid)%cst_uid%value < cst_uid%value) then
                left = mid + 1
            else
                right = mid - 1
            end if
        end do
    end function node_mapping_find_cst_uid
    
    ! Find entry index by AST UID using linear search
    function node_mapping_find_ast_uid(this, ast_uid) result(index)
        class(node_mapping_t), intent(in) :: this
        type(uid_t), intent(in) :: ast_uid
        integer :: index
        
        integer :: i
        
        index = 0  ! Not found
        
        ! Linear search for AST UID
        do i = 1, this%size
            if (uid_equal(this%entries(i)%ast_uid, ast_uid)) then
                index = i
                return
            end if
        end do
    end function node_mapping_find_ast_uid
    
    ! Sort entries by CST UID for binary search
    subroutine node_mapping_sort(this)
        class(node_mapping_t), intent(inout) :: this
        
        integer :: i, j, min_idx
        type(mapping_entry_t) :: temp_entry
        
        if (this%size <= 1) then
            this%is_sorted = .true.
            return
        end if
        
        ! Simple selection sort (good enough for moderate sizes)
        do i = 1, this%size - 1
            min_idx = i
            do j = i + 1, this%size
                if (this%entries(j)%cst_uid%value < this%entries(min_idx)%cst_uid%value) then
                    min_idx = j
                end if
            end do
            
            if (min_idx /= i) then
                temp_entry = this%entries(i)
                this%entries(i) = this%entries(min_idx)
                this%entries(min_idx) = temp_entry
            end if
        end do
        
        this%is_sorted = .true.
    end subroutine node_mapping_sort
    
    ! Remove mapping by CST UID
    function node_mapping_remove(this, cst_uid) result(res)
        class(node_mapping_t), intent(inout) :: this
        type(uid_t), intent(in) :: cst_uid
        type(result_t) :: res
        
        integer :: index, i
        
        ! Find entry to remove
        if (.not. this%is_sorted) call this%sort_entries()
        index = this%find_by_cst_uid(cst_uid)
        
        if (index == 0) then
            res = create_error_result( &
                "CST UID not found in mapping", &
                ERROR_VALIDATION, &
                component="node_mapping", &
                context="remove_mapping", &
                suggestion="Ensure CST UID exists in mapping before removal" &
            )
            return
        end if
        
        ! Shift entries to remove the found entry
        do i = index, this%size - 1
            this%entries(i) = this%entries(i + 1)
        end do
        this%size = this%size - 1
        
        res = success_result()
    end function node_mapping_remove
    
    ! Clear all mappings
    subroutine node_mapping_clear(this)
        class(node_mapping_t), intent(inout) :: this
        
        this%size = 0
        this%current_generation = this%current_generation + 1
        this%is_sorted = .true.
    end subroutine node_mapping_clear
    
    ! Get current size
    pure function node_mapping_get_size(this) result(size)
        class(node_mapping_t), intent(in) :: this
        integer :: size
        
        size = this%size
    end function node_mapping_get_size
    
    ! Check if mapping is empty
    pure function node_mapping_is_empty(this) result(empty)
        class(node_mapping_t), intent(in) :: this
        logical :: empty
        
        empty = this%size == 0
    end function node_mapping_is_empty
    
    ! Validate mapping consistency
    function node_mapping_validate(this) result(res)
        class(node_mapping_t), intent(in) :: this
        type(result_t) :: res
        
        integer :: i
        
        ! Check for duplicate CST UIDs
        do i = 1, this%size - 1
            if (uid_equal(this%entries(i)%cst_uid, this%entries(i + 1)%cst_uid)) then
                res = create_error_result( &
                    "Duplicate CST UID found in mapping", &
                    ERROR_INTERNAL, &
                    component="node_mapping", &
                    context="validate_mapping", &
                    suggestion="Remove duplicate mappings" &
                )
                return
            end if
        end do
        
        res = success_result()
    end function node_mapping_validate
    
    ! Get mapping statistics
    function node_mapping_get_stats(this) result(stats)
        class(node_mapping_t), intent(in) :: this
        type(mapping_stats_t) :: stats
        
        stats%total_mappings = this%size
        stats%memory_usage_bytes = this%capacity * storage_size(this%entries(1)) / 8
        ! Other stats would require tracking across operations
    end function node_mapping_get_stats
    
    ! Resize internal storage
    subroutine node_mapping_resize(this, new_capacity)
        class(node_mapping_t), intent(inout) :: this
        integer, intent(in) :: new_capacity
        
        type(mapping_entry_t), allocatable :: new_entries(:)
        integer :: copy_size
        
        if (new_capacity <= this%capacity) return
        
        allocate(new_entries(new_capacity))
        
        ! Copy existing entries
        copy_size = min(this%size, new_capacity)
        if (copy_size > 0) then
            new_entries(1:copy_size) = this%entries(1:copy_size)
        end if
        
        ! Replace storage
        call move_alloc(new_entries, this%entries)
        this%capacity = new_capacity
    end subroutine node_mapping_resize
    
    ! Assignment operator for mapping entries
    subroutine mapping_entry_assign(lhs, rhs)
        class(mapping_entry_t), intent(out) :: lhs
        type(mapping_entry_t), intent(in) :: rhs
        
        lhs%cst_uid = rhs%cst_uid
        lhs%ast_uid = rhs%ast_uid
        lhs%cst_arena_index = rhs%cst_arena_index
        lhs%ast_arena_index = rhs%ast_arena_index
        lhs%mapping_generation = rhs%mapping_generation
    end subroutine mapping_entry_assign
    
end module node_mapping