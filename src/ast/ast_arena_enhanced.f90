module ast_arena_enhanced
    ! Enhanced AST arena with update capabilities for Issue #360
    ! Extends ast_arena_modern with in-place update operations
    ! Required for full AST migration to modern arena architecture
    
    use ast_arena_modern, only: ast_arena_t, ast_handle_t, ast_node_arena_t, &
                               create_ast_arena, destroy_ast_arena, &
                               store_ast_node, get_ast_node, &
                               is_valid_ast_handle, null_ast_handle, &
                               ast_arena_stats_t
    implicit none
    private
    
    ! Re-export existing functionality
    public :: ast_arena_t, ast_handle_t, ast_node_arena_t
    public :: create_ast_arena, destroy_ast_arena
    public :: store_ast_node, get_ast_node
    public :: is_valid_ast_handle, null_ast_handle
    public :: ast_arena_stats_t
    
    ! New enhanced functionality
    public :: update_ast_node, update_ast_node_field
    public :: link_parent_child, link_siblings
    public :: get_children_handles, get_parent_handle
    public :: traverse_depth_first, traverse_breadth_first
    public :: find_nodes_by_type, find_nodes_by_predicate
    
    ! Enhanced arena type with update capability
    type, extends(ast_arena_t) :: ast_arena_enhanced_t
    contains
        procedure :: update_node => arena_update_node
        procedure :: update_field => arena_update_field
        procedure :: link_nodes => arena_link_nodes
    end type ast_arena_enhanced_t
    
    ! Traversal callback interface
    abstract interface
        logical function node_predicate(node)
            import :: ast_node_arena_t
            type(ast_node_arena_t), intent(in) :: node
        end function node_predicate
        
        subroutine node_visitor(node, handle)
            import :: ast_node_arena_t, ast_handle_t
            type(ast_node_arena_t), intent(in) :: node
            type(ast_handle_t), intent(in) :: handle
        end subroutine node_visitor
    end interface
    
contains
    
    ! Update entire node in place
    subroutine update_ast_node(arena, handle, updated_node)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t), intent(in) :: updated_node
        
        ! Direct update if handle is valid
        if (arena%validate(handle)) then
            ! Access internal storage (requires friend access)
            ! For now, we'll need to extend ast_arena_modern
            ! to expose update capability
            
            ! Placeholder: This needs implementation in ast_arena_modern
            ! arena%nodes(handle%node_id) = updated_node
        end if
    end subroutine update_ast_node
    
    ! Update specific field in node
    subroutine update_ast_node_field(arena, handle, field_name, field_value)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        character(len=*), intent(in) :: field_name
        class(*), intent(in) :: field_value
        type(ast_node_arena_t) :: node
        
        if (.not. arena%validate(handle)) return
        
        ! Get current node
        node = get_ast_node(arena, handle)
        
        ! Update specific field
        select case (field_name)
        case ("node_type_name")
            select type (field_value)
            type is (character(len=*))
                node%node_type_name = field_value
            end select
            
        case ("node_kind")
            select type (field_value)
            type is (integer)
                node%node_kind = field_value
            end select
            
        case ("depth")
            select type (field_value)
            type is (integer)
                node%depth = field_value
            end select
            
        case ("child_count")
            select type (field_value)
            type is (integer)
                node%child_count = field_value
            end select
            
        case ("string_data")
            select type (field_value)
            type is (character(len=*))
                node%string_data = field_value
            end select
            
        case ("integer_data")
            select type (field_value)
            type is (integer)
                node%integer_data = field_value
            end select
            
        case ("boolean_data")
            select type (field_value)
            type is (logical)
                node%boolean_data = field_value
            end select
            
        case ("real_data")
            select type (field_value)
            type is (real)
                node%real_data = field_value
            end select
        end select
        
        ! Update node in arena
        call update_ast_node(arena, handle, node)
    end subroutine update_ast_node_field
    
    ! Link parent and child nodes
    subroutine link_parent_child(arena, parent_handle, child_handle)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: parent_handle, child_handle
        type(ast_node_arena_t) :: parent_node, child_node
        
        if (.not. arena%validate(parent_handle)) return
        if (.not. arena%validate(child_handle)) return
        
        ! Get nodes
        parent_node = get_ast_node(arena, parent_handle)
        child_node = get_ast_node(arena, child_handle)
        
        ! Update parent's first child if needed
        if (parent_node%child_count == 0) then
            parent_node%first_child_id = child_handle%node_id
            parent_node%first_child_gen = child_handle%generation
        end if
        parent_node%child_count = parent_node%child_count + 1
        
        ! Update child's parent
        child_node%parent_handle_id = parent_handle%node_id
        child_node%parent_handle_gen = parent_handle%generation
        
        ! Store updated nodes
        call update_ast_node(arena, parent_handle, parent_node)
        call update_ast_node(arena, child_handle, child_node)
    end subroutine link_parent_child
    
    ! Link sibling nodes
    subroutine link_siblings(arena, prev_handle, next_handle)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: prev_handle, next_handle
        type(ast_node_arena_t) :: prev_node
        
        if (.not. arena%validate(prev_handle)) return
        if (.not. arena%validate(next_handle)) return
        
        ! Get previous node
        prev_node = get_ast_node(arena, prev_handle)
        
        ! Update sibling link
        prev_node%next_sibling_id = next_handle%node_id
        prev_node%next_sibling_gen = next_handle%generation
        
        ! Store updated node
        call update_ast_node(arena, prev_handle, prev_node)
    end subroutine link_siblings
    
    ! Get all children handles
    function get_children_handles(arena, parent_handle) result(children)
        type(ast_arena_t), intent(in) :: arena
        type(ast_handle_t), intent(in) :: parent_handle
        type(ast_handle_t), allocatable :: children(:)
        type(ast_node_arena_t) :: parent_node, current_node
        type(ast_handle_t) :: current_handle
        integer :: i
        
        parent_node = get_ast_node(arena, parent_handle)
        
        allocate(children(parent_node%child_count))
        
        if (parent_node%child_count > 0) then
            ! Start with first child
            current_handle%node_id = parent_node%first_child_id
            current_handle%generation = parent_node%first_child_gen
            
            do i = 1, parent_node%child_count
                children(i) = current_handle
                
                ! Get next sibling
                if (i < parent_node%child_count) then
                    current_node = get_ast_node(arena, current_handle)
                    current_handle%node_id = current_node%next_sibling_id
                    current_handle%generation = current_node%next_sibling_gen
                end if
            end do
        end if
    end function get_children_handles
    
    ! Get parent handle
    function get_parent_handle(arena, child_handle) result(parent_handle)
        type(ast_arena_t), intent(in) :: arena
        type(ast_handle_t), intent(in) :: child_handle
        type(ast_handle_t) :: parent_handle
        type(ast_node_arena_t) :: child_node
        
        child_node = get_ast_node(arena, child_handle)
        
        parent_handle%node_id = child_node%parent_handle_id
        parent_handle%generation = child_node%parent_handle_gen
    end function get_parent_handle
    
    ! Depth-first traversal
    subroutine traverse_depth_first(arena, root_handle, visitor)
        type(ast_arena_t), intent(in) :: arena
        type(ast_handle_t), intent(in) :: root_handle
        procedure(node_visitor) :: visitor
        
        call traverse_dfs_recursive(arena, root_handle, visitor)
    end subroutine traverse_depth_first
    
    recursive subroutine traverse_dfs_recursive(arena, handle, visitor)
        type(ast_arena_t), intent(in) :: arena
        type(ast_handle_t), intent(in) :: handle
        procedure(node_visitor) :: visitor
        type(ast_node_arena_t) :: node
        type(ast_handle_t), allocatable :: children(:)
        integer :: i
        
        if (.not. arena%validate(handle)) return
        
        node = get_ast_node(arena, handle)
        call visitor(node, handle)
        
        children = get_children_handles(arena, handle)
        do i = 1, size(children)
            call traverse_dfs_recursive(arena, children(i), visitor)
        end do
    end subroutine traverse_dfs_recursive
    
    ! Breadth-first traversal
    subroutine traverse_breadth_first(arena, root_handle, visitor)
        type(ast_arena_t), intent(in) :: arena
        type(ast_handle_t), intent(in) :: root_handle
        procedure(node_visitor) :: visitor
        type(ast_handle_t), allocatable :: queue(:), children(:)
        type(ast_node_arena_t) :: node
        integer :: front, back, i
        
        allocate(queue(1000))  ! Initial queue size
        front = 1
        back = 1
        
        queue(back) = root_handle
        
        do while (front <= back)
            node = get_ast_node(arena, queue(front))
            call visitor(node, queue(front))
            
            children = get_children_handles(arena, queue(front))
            do i = 1, size(children)
                back = back + 1
                if (back > size(queue)) then
                    ! Grow queue if needed
                    queue = [queue, queue]  ! Double size
                end if
                queue(back) = children(i)
            end do
            
            front = front + 1
        end do
    end subroutine traverse_breadth_first
    
    ! Find nodes by type
    function find_nodes_by_type(arena, node_type) result(handles)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: node_type
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_handle_t), allocatable :: temp(:)
        integer :: count, capacity
        
        ! This requires iterating through all nodes in arena
        ! For now, return empty array
        allocate(handles(0))
    end function find_nodes_by_type
    
    ! Find nodes by predicate
    function find_nodes_by_predicate(arena, predicate) result(handles)
        type(ast_arena_t), intent(in) :: arena
        procedure(node_predicate) :: predicate
        type(ast_handle_t), allocatable :: handles(:)
        
        ! This requires iterating through all nodes in arena
        ! For now, return empty array
        allocate(handles(0))
    end function find_nodes_by_predicate
    
    ! Enhanced arena methods
    subroutine arena_update_node(this, handle, node)
        class(ast_arena_enhanced_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t), intent(in) :: node
        
        call update_ast_node(this, handle, node)
    end subroutine arena_update_node
    
    subroutine arena_update_field(this, handle, field_name, field_value)
        class(ast_arena_enhanced_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: handle
        character(len=*), intent(in) :: field_name
        class(*), intent(in) :: field_value
        
        call update_ast_node_field(this, handle, field_name, field_value)
    end subroutine arena_update_field
    
    subroutine arena_link_nodes(this, parent, child)
        class(ast_arena_enhanced_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: parent, child
        
        call link_parent_child(this, parent, child)
    end subroutine arena_link_nodes
    
end module ast_arena_enhanced