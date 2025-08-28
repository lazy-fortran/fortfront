module symbol_management
    ! Symbol table and scope analysis functionality
    ! Provides APIs for working with symbols and scopes
    
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t
    use type_system_unified, only: mono_type_t, poly_type_t
    use fortfront_types, only: symbol_info_t, symbol_reference_t, scope_info_t
    
    implicit none
    private
    
    ! Public symbol management functions
    public :: lookup_symbol, get_symbol_info, get_symbols_in_scope, &
              get_symbol_references, get_scope_info, get_all_scopes, &
              get_scope_symbols
    
contains

    ! Lookup symbol in semantic context
    function lookup_symbol(ctx, name, scope_node_index) result(symbol)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: scope_node_index
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        
        symbol%name = name
        symbol%is_defined = .false.
        symbol%scope_level = 0
        symbol%definition_line = 0
        symbol%definition_column = 0
        
        ! Look up in current scope stack
        call ctx%scopes%lookup(name, scheme)
        
        if (allocated(scheme)) then
            symbol%is_defined = .true.
            symbol%type_info = ctx%instantiate(scheme)
            symbol%scope_level = ctx%scopes%depth
        end if
    end function lookup_symbol

    ! Get detailed symbol information
    function get_symbol_info(ctx, name, scope_level) result(symbol)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: scope_level
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        integer :: original_depth, target_depth
        
        symbol%name = name
        symbol%is_defined = .false.
        symbol%scope_level = scope_level
        symbol%definition_line = 0
        symbol%definition_column = 0
        
        ! Save current depth
        original_depth = ctx%scopes%depth
        
        ! Adjust to target scope level if different
        target_depth = min(scope_level, ctx%scopes%depth)
        if (target_depth > 0) then
            ! Look up in target scope
            call ctx%scopes%lookup_at_level(name, target_depth, scheme)
            
            if (allocated(scheme)) then
                symbol%is_defined = .true.
                symbol%type_info = ctx%instantiate(scheme)
                call check_parameter_attributes(ctx, name, symbol)
            end if
        end if
    end function get_symbol_info

    ! Get all symbols in a scope level
    function get_symbols_in_scope(ctx, scope_level) result(symbols)
        type(semantic_context_t), intent(inout) :: ctx
        integer, intent(in) :: scope_level
        type(symbol_info_t), allocatable :: symbols(:)
        integer :: i, count, target_level
        
        target_level = min(scope_level, ctx%scopes%depth)
        
        if (target_level <= 0) then
            allocate(symbols(0))
            return
        end if
        
        ! Get count of symbols in target scope
        count = ctx%scopes%scopes(target_level)%env%count
        
        if (count <= 0) then
            allocate(symbols(0))
            return
        end if
        
        ! Allocate and populate symbols
        allocate(symbols(count))
        
        do i = 1, count
            symbols(i)%name = ctx%scopes%scopes(target_level)%env%names(i)
            symbols(i)%is_defined = .true.
            symbols(i)%scope_level = target_level
            symbols(i)%type_info = ctx%instantiate( &
                ctx%scopes%scopes(target_level)%env%schemes(i))
            symbols(i)%definition_line = 0
            symbols(i)%definition_column = 0
            call check_parameter_attributes(ctx, symbols(i)%name, symbols(i))
        end do
    end function get_symbols_in_scope

    ! Get all references to a symbol
    function get_symbol_references(arena, ctx, symbol_name) result(references)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: symbol_name
        type(symbol_reference_t), allocatable :: references(:)
        integer :: i, ref_count
        type(symbol_reference_t), allocatable :: temp_refs(:)
        
        ! First pass: count references
        ref_count = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                class is (ast_node)
                    if (node_references_symbol(node, symbol_name)) then
                        ref_count = ref_count + 1
                    end if
                end select
            end if
        end do
        
        if (ref_count == 0) then
            allocate(references(0))
            return
        end if
        
        ! Second pass: populate references
        allocate(temp_refs(ref_count))
        ref_count = 0
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                class is (ast_node)
                    if (node_references_symbol(node, symbol_name)) then
                        ref_count = ref_count + 1
                        temp_refs(ref_count)%node_index = i
                        temp_refs(ref_count)%symbol_name = symbol_name
                        temp_refs(ref_count)%reference_type = get_reference_type(node)
                        temp_refs(ref_count)%line_number = 0  ! TODO: get from node
                        temp_refs(ref_count)%column_number = 0  ! TODO: get from node
                    end if
                end select
            end if
        end do
        
        ! Move to result
        call move_alloc(temp_refs, references)
    end function get_symbol_references

    ! Get scope information for a level
    function get_scope_info(ctx, scope_level) result(scope_info)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_level
        type(scope_info_t) :: scope_info
        
        scope_info%level = scope_level
        scope_info%symbol_count = 0
        scope_info%parent_level = 0
        scope_info%scope_type = "unknown"
        
        if (scope_level > 0 .and. scope_level <= ctx%scopes%depth) then
            scope_info%symbol_count = ctx%scopes%scopes(scope_level)%env%count
            if (scope_level > 1) then
                scope_info%parent_level = scope_level - 1
            end if
            scope_info%scope_type = get_scope_type_string(ctx%scopes%scopes(scope_level)%scope_type)
        end if
    end function get_scope_info

    ! Get information for all scopes
    function get_all_scopes(ctx) result(scopes)
        type(semantic_context_t), intent(in) :: ctx
        type(scope_info_t), allocatable :: scopes(:)
        integer :: i
        
        allocate(scopes(ctx%scopes%depth))
        
        do i = 1, ctx%scopes%depth
            scopes(i) = get_scope_info(ctx, i)
        end do
    end function get_all_scopes

    ! Helper subroutine to check parameter attributes
    subroutine check_parameter_attributes(ctx, name, symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        type(symbol_info_t), intent(inout) :: symbol
        
        ! Check if this symbol is a parameter
        symbol%is_parameter = ctx%param_tracker%is_parameter(name)
        
        ! Get intent if it's a parameter
        if (symbol%is_parameter) then
            symbol%intent = ctx%param_tracker%get_parameter_intent(name)
        else
            symbol%intent = ""
        end if
    end subroutine check_parameter_attributes

    ! Get symbols in a specific scope node
    function get_scope_symbols(ctx, scope_node_index) result(symbols)
        type(semantic_context_t), intent(inout) :: ctx
        integer, intent(in) :: scope_node_index
        type(symbol_info_t), allocatable :: symbols(:)
        integer :: scope_level
        
        ! For now, map scope_node_index to current depth
        ! This is a simplification - proper implementation would
        ! need to track scope nodes to scope levels mapping
        scope_level = ctx%scopes%depth
        
        symbols = get_symbols_in_scope(ctx, scope_level)
    end function get_scope_symbols

    ! Helper functions
    
    function node_references_symbol(node, symbol_name) result(references)
        class(ast_node), intent(in) :: node
        character(len=*), intent(in) :: symbol_name
        logical :: references
        
        references = .false.
        
        select type (node)
        type is (identifier_node)
            if (allocated(node%name) .and. node%name == symbol_name) then
                references = .true.
            end if
        type is (call_or_subscript_node)
            if (allocated(node%name) .and. node%name == symbol_name) then
                references = .true.
            end if
        end select
    end function node_references_symbol

    function get_reference_type(node) result(ref_type)
        class(ast_node), intent(in) :: node
        character(len=:), allocatable :: ref_type
        
        select type (node)
        type is (identifier_node)
            ref_type = "identifier"
        type is (call_or_subscript_node)
            ref_type = "call"
        type is (assignment_node)
            ref_type = "assignment"
        class default
            ref_type = "unknown"
        end select
    end function get_reference_type

    function get_scope_type_string(scope_type) result(type_str)
        integer, intent(in) :: scope_type
        character(len=:), allocatable :: type_str
        
        select case (scope_type)
        case (0)
            type_str = "global"
        case (1)
            type_str = "module"
        case (2)
            type_str = "function"
        case (3)
            type_str = "subroutine"
        case (4)
            type_str = "block"
        case default
            type_str = "unknown"
        end select
    end function get_scope_type_string

end module symbol_management