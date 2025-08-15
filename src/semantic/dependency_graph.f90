module dependency_graph
    implicit none
    private
    
    public :: dependency_node_t, dependency_graph_t
    public :: create_dependency_graph
    
    ! Node in dependency graph
    type :: dependency_node_t
        character(len=32) :: name = ""
        character(len=32), allocatable :: dependencies(:)
        logical :: visited = .false.
        logical :: in_progress = .false.
    end type
    
    ! Dependency graph for topological sorting
    type :: dependency_graph_t
        type(dependency_node_t), allocatable :: nodes(:)
        integer :: node_count = 0
    contains
        procedure :: add_node
        procedure :: topological_sort
        procedure :: validate_no_cycles
        procedure :: get_execution_order
        procedure :: find_node_index
    end type

contains

    function create_dependency_graph() result(graph)
        type(dependency_graph_t) :: graph
        
        graph%node_count = 0
        allocate(graph%nodes(0))
    end function

    subroutine add_node(this, name, dependencies)
        class(dependency_graph_t), intent(inout) :: this
        character(*), intent(in) :: name
        character(len=32), intent(in) :: dependencies(:)
        
        type(dependency_node_t), allocatable :: temp_nodes(:)
        integer :: i
        
        ! Grow nodes array
        allocate(temp_nodes(this%node_count + 1))
        
        ! Copy existing nodes
        do i = 1, this%node_count
            temp_nodes(i) = this%nodes(i)
        end do
        
        ! Add new node
        temp_nodes(this%node_count + 1)%name = trim(name)
        allocate(temp_nodes(this%node_count + 1)%dependencies(size(dependencies)))
        temp_nodes(this%node_count + 1)%dependencies = dependencies
        temp_nodes(this%node_count + 1)%visited = .false.
        temp_nodes(this%node_count + 1)%in_progress = .false.
        
        ! Update graph
        call move_alloc(temp_nodes, this%nodes)
        this%node_count = this%node_count + 1
    end subroutine

    function find_node_index(this, name) result(index)
        class(dependency_graph_t), intent(in) :: this
        character(*), intent(in) :: name
        integer :: index
        
        integer :: i
        
        index = 0
        do i = 1, this%node_count
            if (trim(this%nodes(i)%name) == trim(name)) then
                index = i
                return
            end if
        end do
    end function

    function validate_no_cycles(this) result(valid)
        class(dependency_graph_t), intent(inout) :: this
        logical :: valid
        
        integer :: i
        
        ! Reset visit flags
        do i = 1, this%node_count
            this%nodes(i)%visited = .false.
            this%nodes(i)%in_progress = .false.
        end do
        
        ! Check each node for cycles using DFS
        valid = .true.
        do i = 1, this%node_count
            if (.not. this%nodes(i)%visited) then
                if (.not. dfs_cycle_check(this, i)) then
                    valid = .false.
                    return
                end if
            end if
        end do
    end function

    recursive function dfs_cycle_check(graph, node_index) result(no_cycle)
        type(dependency_graph_t), intent(inout) :: graph
        integer, intent(in) :: node_index
        logical :: no_cycle
        
        integer :: i, dep_index
        
        ! Mark current node as in progress
        graph%nodes(node_index)%in_progress = .true.
        
        ! Check all dependencies
        do i = 1, size(graph%nodes(node_index)%dependencies)
            dep_index = graph%find_node_index( &
                graph%nodes(node_index)%dependencies(i))
            
            if (dep_index > 0) then
                ! If dependency is in progress, we have a cycle
                if (graph%nodes(dep_index)%in_progress) then
                    no_cycle = .false.
                    return
                end if
                
                ! If not visited, recursively check
                if (.not. graph%nodes(dep_index)%visited) then
                    if (.not. dfs_cycle_check(graph, dep_index)) then
                        no_cycle = .false.
                        return
                    end if
                end if
            end if
        end do
        
        ! Mark as visited and no longer in progress
        graph%nodes(node_index)%visited = .true.
        graph%nodes(node_index)%in_progress = .false.
        no_cycle = .true.
    end function

    function topological_sort(this) result(sorted_names)
        class(dependency_graph_t), intent(inout) :: this
        character(len=32), allocatable :: sorted_names(:)
        
        integer, allocatable :: in_degree(:)
        integer, allocatable :: queue(:)
        integer :: queue_start, queue_end, queue_size
        integer :: i, j, dep_index, current_node
        character(len=32), allocatable :: temp_result(:)
        integer :: result_count
        
        if (this%node_count == 0) then
            allocate(sorted_names(0))
            return
        end if
        
        ! Calculate in-degrees for each node
        allocate(in_degree(this%node_count))
        in_degree = 0
        
        do i = 1, this%node_count
            do j = 1, size(this%nodes(i)%dependencies)
                dep_index = this%find_node_index(this%nodes(i)%dependencies(j))
                if (dep_index > 0) then
                    in_degree(i) = in_degree(i) + 1
                end if
            end do
        end do
        
        ! Initialize queue with nodes having no dependencies
        allocate(queue(this%node_count))
        queue_start = 1
        queue_end = 0
        queue_size = 0
        
        do i = 1, this%node_count
            if (in_degree(i) == 0) then
                queue_end = queue_end + 1
                queue(queue_end) = i
                queue_size = queue_size + 1
            end if
        end do
        
        ! Process nodes in topological order
        allocate(temp_result(this%node_count))
        result_count = 0
        
        do while (queue_size > 0)
            ! Remove node from queue
            current_node = queue(queue_start)
            queue_start = queue_start + 1
            queue_size = queue_size - 1
            
            ! Add to result
            result_count = result_count + 1
            temp_result(result_count) = this%nodes(current_node)%name
            
            ! Find nodes that depend on current node and reduce their in-degree
            do i = 1, this%node_count
                do j = 1, size(this%nodes(i)%dependencies)
                    if (trim(this%nodes(i)%dependencies(j)) == &
                        trim(this%nodes(current_node)%name)) then
                        in_degree(i) = in_degree(i) - 1
                        if (in_degree(i) == 0) then
                            queue_end = queue_end + 1
                            queue(queue_end) = i
                            queue_size = queue_size + 1
                        end if
                    end if
                end do
            end do
        end do
        
        ! Return result
        allocate(sorted_names(result_count))
        sorted_names(1:result_count) = temp_result(1:result_count)
    end function

    function get_execution_order(this) result(order)
        class(dependency_graph_t), intent(inout) :: this
        character(len=32), allocatable :: order(:)
        
        ! Validate no cycles first
        if (.not. this%validate_no_cycles()) then
            allocate(order(0))
            return
        end if
        
        ! Return topological sort order
        order = this%topological_sort()
    end function

end module dependency_graph