module cfg_builder_types
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena_modern
    use control_flow_graph_module
    implicit none
    private

    ! Constants for constant folding results
    integer, parameter, public :: CONSTANT_UNKNOWN = 0
    integer, parameter, public :: CONSTANT_TRUE = 1
    integer, parameter, public :: CONSTANT_FALSE = 2

    ! CFG builder state
    type, public :: cfg_builder_t
        type(control_flow_graph_t) :: cfg
        integer :: current_block_id = 0
        integer, allocatable :: statement_buffer(:)
        integer :: buffer_size = 0
        integer :: buffer_capacity = 16
    end type cfg_builder_t
    
    ! Iterative traversal frame for CFG builder
    type, public :: cfg_frame_t
        integer :: node_index = 0
        integer :: state = 0
        integer :: entry_block_id = 0
        integer :: exit_block_id = 0
        integer :: aux_block_id = 0
        integer :: aux_data = 0
        integer, allocatable :: pending_indices(:)
        integer :: pending_position = 0
        integer :: pending_count = 0
    end type cfg_frame_t

    type, public :: cfg_frame_stack_t
        type(cfg_frame_t), allocatable :: items(:)
        integer :: top = 0
    end type cfg_frame_stack_t

    ! Procedure pointer interface for process_node
    abstract interface
        recursive subroutine process_node_interface(builder, arena, node_index)
            import :: cfg_builder_t, ast_arena_t
            type(cfg_builder_t), intent(inout) :: builder
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
        end subroutine
    end interface
    
    ! Public interface type
    public :: process_node_interface

end module cfg_builder_types
