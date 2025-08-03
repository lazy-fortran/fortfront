module call_graph_builder_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    use ast_visitor
    use ast_traversal, only: traverse_ast
    use call_graph_module
    implicit none
    private

    ! Public interface
    public :: call_graph_builder_t, create_call_graph_builder
    public :: build_call_graph

    ! Call graph builder (non-visitor approach)
    type :: call_graph_builder_t
        type(call_graph_t) :: graph
        character(len=256) :: current_scope = ""
        integer :: scope_depth = 0
    end type call_graph_builder_t

contains

    ! Create a new call graph builder
    function create_call_graph_builder() result(builder)
        type(call_graph_builder_t) :: builder
        builder%graph = create_call_graph()
        builder%current_scope = ""
        builder%scope_depth = 0
    end function create_call_graph_builder

    ! Main entry point: build call graph from AST
    function build_call_graph(arena, root_index) result(graph)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph
        
        type(call_graph_builder_t) :: builder
        
        ! Create builder and traverse AST
        builder = create_call_graph_builder()
        call traverse_for_calls(builder, arena, root_index, "")
        
        ! Return the built graph
        graph = builder%graph
    end function build_call_graph

    ! Recursive traversal for call graph construction
    recursive subroutine traverse_for_calls(builder, arena, node_index, current_scope)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: current_scope
        
        character(len=:), allocatable :: node_type
        character(len=256) :: new_scope
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        select case (node_type)
        case ("program")
            ! Handle program node
            select type (node => arena%entries(node_index)%node)
            type is (program_node)
                call builder%graph%add_proc(node%name, node_index, &
                                           node%line, node%column, is_main=.true.)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_for_calls(builder, arena, &
                                               node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("function_def")
            ! Handle function definition
            select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
                call builder%graph%add_proc(node%name, node_index, &
                                           node%line, node%column)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_for_calls(builder, arena, &
                                               node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("subroutine_def")
            ! Handle subroutine definition
            select type (node => arena%entries(node_index)%node)
            type is (subroutine_def_node)
                call builder%graph%add_proc(node%name, node_index, &
                                           node%line, node%column)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_for_calls(builder, arena, &
                                               node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("subroutine_call")
            ! Handle subroutine call
            select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
                if (len_trim(current_scope) > 0) then
                    call builder%graph%add_call_edge(current_scope, node%name, &
                                                   node_index, node%line, node%column)
                end if
            end select
            
        case ("call_or_subscript")
            ! Handle function call (need to check if it's actually a call)
            select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
                ! For now, assume it's a function call if in expression context
                if (len_trim(current_scope) > 0) then
                    call builder%graph%add_call_edge(current_scope, node%name, &
                                                   node_index, node%line, node%column)
                end if
            end select
            
        case ("assignment")
            ! Handle assignment nodes - traverse both target and value
            select type (node => arena%entries(node_index)%node)
            type is (assignment_node)
                ! Visit target and value expressions
                call traverse_for_calls(builder, arena, node%target_index, current_scope)
                call traverse_for_calls(builder, arena, node%value_index, current_scope)
            end select
            
        case ("binary_op")
            ! Handle binary operation nodes - traverse both operands
            select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
                ! Visit left and right operands
                call traverse_for_calls(builder, arena, node%left_index, current_scope)
                call traverse_for_calls(builder, arena, node%right_index, current_scope)
            end select
            
        case ("module")
            ! Handle module node
            select type (node => arena%entries(node_index)%node)
            type is (module_node)
                ! Visit module declarations
                if (allocated(node%declaration_indices)) then
                    do i = 1, size(node%declaration_indices)
                        call traverse_for_calls(builder, arena, &
                                               node%declaration_indices(i), "")
                    end do
                end if
                
                ! Visit module procedures
                if (allocated(node%procedure_indices)) then
                    do i = 1, size(node%procedure_indices)
                        call traverse_for_calls(builder, arena, &
                                               node%procedure_indices(i), "")
                    end do
                end if
            end select
            
        case default
            ! For other node types, just traverse children
            call traverse_children_for_calls(builder, arena, node_index, current_scope)
        end select
    end subroutine traverse_for_calls
    
    ! Helper to traverse all children of a node
    subroutine traverse_children_for_calls(builder, arena, node_index, current_scope)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: current_scope
        
        integer, allocatable :: children(:)
        integer :: i
        
        ! Get children indices based on node type
        children = arena%get_children(node_index)
        
        if (allocated(children)) then
            do i = 1, size(children)
                call traverse_for_calls(builder, arena, children(i), current_scope)
            end do
        end if
    end subroutine traverse_children_for_calls

end module call_graph_builder_module