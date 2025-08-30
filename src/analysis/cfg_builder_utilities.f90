module cfg_builder_utilities
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena_modern
    implicit none
    private

    ! Public interface
    public :: check_allocate_exception_params, check_deallocate_exception_params
    public :: check_io_exception_params

contains

    ! Helper function to check allocate statement exception parameters
    subroutine check_allocate_exception_params(arena, node_index, has_stat, has_errmsg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_stat, has_errmsg
        
        has_stat = .false.
        has_errmsg = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check node type before accessing fields
        if (arena%entries(node_index)%node_type == "allocate_statement" .or. &
            arena%entries(node_index)%node_type == "allocate_node") then
            select type (node => arena%entries(node_index)%node)
            type is (allocate_statement_node)
                has_stat = (node%stat_var_index > 0)
                has_errmsg = (node%errmsg_var_index > 0)
            end select
        end if
    end subroutine check_allocate_exception_params
    
    ! Helper function to check deallocate statement exception parameters
    subroutine check_deallocate_exception_params(arena, node_index, has_stat, has_errmsg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_stat, has_errmsg
        
        has_stat = .false.
        has_errmsg = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check node type before accessing fields
        if (arena%entries(node_index)%node_type == "deallocate_statement" .or. &
            arena%entries(node_index)%node_type == "deallocate_node") then
            select type (node => arena%entries(node_index)%node)
            type is (deallocate_statement_node)
                has_stat = (node%stat_var_index > 0)
                has_errmsg = (node%errmsg_var_index > 0)
            end select
        end if
    end subroutine check_deallocate_exception_params
    
    ! Helper function to check I/O statement exception parameters
    subroutine check_io_exception_params(arena, node_index, has_iostat, has_err, has_end)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_iostat, has_err, has_end
        
        has_iostat = .false.
        has_err = .false.
        has_end = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select case (arena%entries(node_index)%node_type)
        case ("read_statement", "read_node")
            select type (node => arena%entries(node_index)%node)
            type is (read_statement_node)
                has_iostat = (node%iostat_var_index > 0)
                has_err = (node%err_label_index > 0)
                has_end = (node%end_label_index > 0)
            class default
                ! Node type mismatch - silently return defaults
            end select
        case ("write_statement", "write_node")
            select type (node => arena%entries(node_index)%node)
            type is (write_statement_node)
                has_iostat = (node%iostat_var_index > 0)
                has_err = (node%err_label_index > 0)
                has_end = (node%end_label_index > 0)
            class default
                ! Node type mismatch - silently return defaults
            end select
        case default
            ! Unsupported node type - return defaults
        end select
    end subroutine check_io_exception_params

end module cfg_builder_utilities