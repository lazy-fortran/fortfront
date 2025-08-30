module cfg_builder_core
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena_modern
    use control_flow_graph_module
    use conditional_evaluation_module, only: evaluate_constant_condition
    use cfg_builder_types
    use cfg_builder_helpers
    implicit none
    private

    ! Public interface
    public :: cfg_builder_t, create_cfg_builder
    public :: build_control_flow_graph
    public :: CONSTANT_UNKNOWN, CONSTANT_TRUE, CONSTANT_FALSE
    
    ! Re-export helpers
    public :: add_statement_to_buffer, flush_statement_buffer

contains

    ! Create a new CFG builder
    function create_cfg_builder() result(builder)
        type(cfg_builder_t) :: builder
        
        builder%cfg = create_control_flow_graph()
        builder%current_block_id = 0
        allocate(builder%statement_buffer(builder%buffer_capacity))
        builder%buffer_size = 0
    end function create_cfg_builder

    ! Main entry point: build CFG from AST
    function build_control_flow_graph(arena, root_index) result(cfg)
        use cfg_builder_control_handlers, only: process_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(control_flow_graph_t) :: cfg
        
        type(cfg_builder_t) :: builder
        character(len=:), allocatable :: proc_name
        
        ! Create builder
        builder = create_cfg_builder()
        
        ! Get procedure name if available
        if (allocated(arena%entries(root_index)%node)) then
            select case (arena%entries(root_index)%node_type)
            case ("program")
                select type (node => arena%entries(root_index)%node)
                type is (program_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case ("function_def")
                select type (node => arena%entries(root_index)%node)
                type is (function_def_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case ("subroutine_def")
                select type (node => arena%entries(root_index)%node)
                type is (subroutine_def_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case default
                proc_name = "anonymous"
            end select
            
            builder%cfg%procedure_name = proc_name
        end if
        
        ! Create entry block
        builder%current_block_id = add_basic_block(builder%cfg, "entry", &
                                                  is_entry=.true.)
        
        ! Process the AST
        call process_node(builder, arena, root_index)
        
        ! Flush any remaining statements
        call flush_statement_buffer(builder)
        
        ! Find reachable blocks
        call find_reachable_blocks(builder%cfg)
        
        ! Return the built CFG
        cfg = builder%cfg
    end function build_control_flow_graph

end module cfg_builder_core