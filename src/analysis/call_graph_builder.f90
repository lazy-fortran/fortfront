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

    ! Symbol table entry for procedure resolution
    type :: proc_symbol_t
        character(len=:), allocatable :: name       ! Simple name
        character(len=:), allocatable :: full_name  ! Fully qualified name
        character(len=:), allocatable :: scope      ! Parent scope
    end type proc_symbol_t
    
    ! Call graph builder (non-visitor approach)
    type :: call_graph_builder_t
        type(call_graph_t) :: graph
        character(len=256) :: current_scope = ""
        integer :: scope_depth = 0
        type(proc_symbol_t), allocatable :: symbol_table(:)
        integer :: symbol_count = 0
    end type call_graph_builder_t

contains

    ! Create a new call graph builder
    function create_call_graph_builder() result(builder)
        type(call_graph_builder_t) :: builder
        builder%graph = create_call_graph()
        builder%current_scope = ""
        builder%scope_depth = 0
        allocate(builder%symbol_table(256))  ! Initial capacity
        builder%symbol_count = 0
    end function create_call_graph_builder

    ! Main entry point: build call graph from AST
    function build_call_graph(arena, root_index) result(graph)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph
        
        type(call_graph_builder_t) :: builder
        integer :: i
        
        ! Create builder 
        builder = create_call_graph_builder()
        
        ! Always traverse from root first to handle single compilation units
        if (root_index > 0) then
            call traverse_for_calls(builder, arena, root_index, "")
        end if
        
        ! Additionally, for multi-unit compilation (modules + programs),
        ! traverse all top-level module and program nodes that aren't the root
        if (allocated(arena%entries)) then
            do i = 1, min(arena%size, size(arena%entries))
                if (i /= root_index .and. allocated(arena%entries(i)%node)) then
                    select case (arena%entries(i)%node_type)
                    case ("module", "module_node", "program")
                        ! Traverse additional modules and programs
                        call traverse_for_calls(builder, arena, i, "")
                    end select
                end if
            end do
        end if
        
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
                call add_to_symbol_table(builder, node%name, node%name, "")
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
                ! Build scope name for nested procedures
                if (len_trim(current_scope) > 0) then
                    new_scope = trim(current_scope) // "::" // node%name
                else
                    new_scope = node%name
                end if
                
                call builder%graph%add_proc(new_scope, node_index, &
                                           node%line, node%column)
                call add_to_symbol_table(builder, node%name, new_scope, current_scope)
                
                ! Traverse body (includes nested procedures in contains section)
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
                ! Build scope name for nested procedures
                if (len_trim(current_scope) > 0) then
                    new_scope = trim(current_scope) // "::" // node%name
                else
                    new_scope = node%name
                end if
                
                call builder%graph%add_proc(new_scope, node_index, &
                                           node%line, node%column)
                call add_to_symbol_table(builder, node%name, new_scope, current_scope)
                
                ! Traverse body (includes nested procedures in contains section)
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
                    block
                        character(len=256) :: resolved_name
                        ! Resolve the procedure name using symbol table
                        resolved_name = resolve_procedure_name(builder, node%name, current_scope)
                        call builder%graph%add_call_edge(current_scope, resolved_name, &
                                                       node_index, node%line, node%column)
                    end block
                end if
            end select
            
        case ("call_or_subscript")
            ! Handle function call (need to check if it's actually a call)
            select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
                ! For now, assume it's a function call if in expression context
                if (len_trim(current_scope) > 0) then
                    block
                        character(len=256) :: resolved_name
                        ! Resolve the procedure name using symbol table
                        resolved_name = resolve_procedure_name(builder, node%name, current_scope)
                        call builder%graph%add_call_edge(current_scope, resolved_name, &
                                                       node_index, node%line, node%column)
                    end block
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
            
        case ("module", "module_node")
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
            
        case ("contains", "contains_section")
            ! Handle contains section - traverse all contained procedures
            call traverse_children_for_calls(builder, arena, node_index, current_scope)
            
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

    ! Add a procedure to the symbol table
    subroutine add_to_symbol_table(builder, simple_name, full_name, scope)
        type(call_graph_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: simple_name, full_name, scope
        
        type(proc_symbol_t), allocatable :: temp_table(:)
        
        ! Expand table if needed
        if (builder%symbol_count >= size(builder%symbol_table)) then
            allocate(temp_table(size(builder%symbol_table) * 2))
            temp_table(1:builder%symbol_count) = builder%symbol_table(1:builder%symbol_count)
            call move_alloc(temp_table, builder%symbol_table)
        end if
        
        ! Add new symbol
        builder%symbol_count = builder%symbol_count + 1
        builder%symbol_table(builder%symbol_count)%name = simple_name
        builder%symbol_table(builder%symbol_count)%full_name = full_name
        builder%symbol_table(builder%symbol_count)%scope = scope
    end subroutine add_to_symbol_table
    
    ! Resolve a procedure name to its fully qualified name
    function resolve_procedure_name(builder, simple_name, calling_scope) result(resolved_name)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: simple_name
        character(len=*), intent(in) :: calling_scope
        character(len=256) :: resolved_name
        
        character(len=256) :: search_scope
        integer :: i, last_sep
        logical :: found
        
        ! First, check for nested procedure in current scope hierarchy
        search_scope = calling_scope
        found = .false.
        
        do while (len_trim(search_scope) > 0 .and. .not. found)
            ! Look for simple_name in search_scope
            do i = 1, builder%symbol_count
                if (builder%symbol_table(i)%name == simple_name .and. &
                    builder%symbol_table(i)%scope == search_scope) then
                    resolved_name = builder%symbol_table(i)%full_name
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                ! Move up one scope level
                last_sep = index(search_scope, "::", back=.true.)
                if (last_sep > 0) then
                    search_scope = search_scope(1:last_sep-1)
                else
                    search_scope = ""
                end if
            end if
        end do
        
        ! If not found in nested scopes, check for top-level procedure
        if (.not. found) then
            do i = 1, builder%symbol_count
                if (builder%symbol_table(i)%name == simple_name .and. &
                    len_trim(builder%symbol_table(i)%scope) == 0) then
                    resolved_name = builder%symbol_table(i)%full_name
                    found = .true.
                    exit
                end if
            end do
        end if
        
        ! If still not found, return the simple name (external procedure)
        if (.not. found) then
            resolved_name = simple_name
        end if
    end function resolve_procedure_name

end module call_graph_builder_module