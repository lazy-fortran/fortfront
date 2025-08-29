module variable_usage_core_module
    use iso_fortran_env, only: error_unit
    use ast_arena_modern
    implicit none
    private

    ! Public types
    public :: variable_usage_info_t, expression_visitor_t
    
    ! Public procedures  
    public :: create_variable_usage_info, add_variable_to_info_common
    public :: add_identifier_to_info, add_string_to_info

    ! Variable usage information
    type :: variable_usage_info_t
        character(len=:), allocatable :: variable_names(:)
        integer, allocatable :: usage_counts(:)
        integer, allocatable :: node_indices(:)
        integer :: total_count = 0
    contains
        procedure :: assign_usage_info
        generic :: assignment(=) => assign_usage_info
    end type variable_usage_info_t

    ! Expression visitor interface
    abstract interface
        subroutine expression_visitor_interface(arena, node_index, node_type, &
                                               user_data)
            import :: ast_arena_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            character(len=*), intent(in) :: node_type
            class(*), intent(inout), optional :: user_data
        end subroutine expression_visitor_interface
    end interface

    type :: expression_visitor_t
        procedure(expression_visitor_interface), pointer, nopass :: visit => null()
    end type expression_visitor_t

contains

    ! Create empty variable usage info
    function create_variable_usage_info() result(info)
        type(variable_usage_info_t) :: info
        
        ! Leave arrays unallocated initially
        ! They will be allocated when first variable is added
        info%total_count = 0
    end function create_variable_usage_info

    ! Add an identifier to the usage info
    subroutine add_identifier_to_info(arena, node_index, info)
        use ast_nodes_core, only: identifier_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        character(len=:), allocatable :: var_name
        
        ! Get variable name from identifier node
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            var_name = node%name
        class default
            return
        end select
        
        ! Use common helper
        call add_variable_to_info_common(var_name, node_index, info)
    end subroutine add_identifier_to_info

    ! Common helper to add a variable name to usage info
    subroutine add_variable_to_info_common(var_name, node_index, info)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i, existing_index
        
        ! Check if variable already exists in the list
        existing_index = 0
        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    existing_index = i
                    exit
                end if
            end do
        end if
        
        if (existing_index > 0) then
            ! Increment existing count
            info%usage_counts(existing_index) = info%usage_counts(existing_index) + 1
        else
            ! Add new variable
            if (.not. allocated(info%variable_names)) then
                ! First variable - allocate new arrays
                allocate(character(len=len(var_name)) :: info%variable_names(1))
                info%variable_names(1) = var_name
                allocate(info%usage_counts(1))
                info%usage_counts(1) = 1
                allocate(info%node_indices(1))
                info%node_indices(1) = node_index
            else
                ! Extend existing arrays
                block
                    character(len=:), allocatable :: temp_names(:)
                    integer, allocatable :: temp_counts(:), temp_indices(:)
                    integer :: n
                    
                    n = size(info%variable_names)
                    allocate(character(len=max(len(info%variable_names), len(var_name))) :: temp_names(n+1))
                    allocate(temp_counts(n+1))
                    allocate(temp_indices(n+1))
                    
                    temp_names(1:n) = info%variable_names
                    temp_names(n+1) = var_name
                    temp_counts(1:n) = info%usage_counts
                    temp_counts(n+1) = 1
                    temp_indices(1:n) = info%node_indices
                    temp_indices(n+1) = node_index
                    
                    call move_alloc(temp_names, info%variable_names)
                    call move_alloc(temp_counts, info%usage_counts)
                    call move_alloc(temp_indices, info%node_indices)
                end block
            end if
        end if
        
        info%total_count = info%total_count + 1
    end subroutine add_variable_to_info_common

    ! Add a string-based variable name to the usage info
    subroutine add_string_to_info(var_name, node_index, info)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Use common helper
        call add_variable_to_info_common(var_name, node_index, info)
    end subroutine add_string_to_info

    ! Assignment operator for variable_usage_info_t to handle deep copying
    subroutine assign_usage_info(lhs, rhs)
        class(variable_usage_info_t), intent(inout) :: lhs
        type(variable_usage_info_t), intent(in) :: rhs
        
        ! Clear any existing allocations
        if (allocated(lhs%variable_names)) deallocate(lhs%variable_names)
        if (allocated(lhs%usage_counts)) deallocate(lhs%usage_counts)
        if (allocated(lhs%node_indices)) deallocate(lhs%node_indices)
        
        ! Deep copy allocatable arrays
        if (allocated(rhs%variable_names)) then
            allocate(character(len(rhs%variable_names)) :: lhs%variable_names(size(rhs%variable_names)))
            lhs%variable_names = rhs%variable_names
        end if
        
        if (allocated(rhs%usage_counts)) then
            allocate(lhs%usage_counts(size(rhs%usage_counts)))
            lhs%usage_counts = rhs%usage_counts
        end if
        
        if (allocated(rhs%node_indices)) then
            allocate(lhs%node_indices(size(rhs%node_indices)))
            lhs%node_indices = rhs%node_indices
        end if
        
        ! Copy scalar value
        lhs%total_count = rhs%total_count
    end subroutine assign_usage_info

end module variable_usage_core_module