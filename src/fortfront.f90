module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    !
    ! IMPORTANT: AST Node Access Policy
    ! =================================
    ! AST nodes MUST NOT be copied due to complex allocatable components
    ! that can cause memory corruption and segmentation faults.
    ! 
    ! USE ONLY the visitor pattern for safe node access:
    !   - visit_node_at() for visiting nodes by index
    !   - AST traversal functions with custom visitors
    !
    ! DO NOT attempt to:
    !   - Copy nodes with allocate(source=...)
    !   - Create functions that return node copies
    !   - Perform shallow copies of nodes
    !
    ! For read-only access to node properties, use:
    !   - get_node_type_id_from_arena()
    !   - get_node_source_location_from_arena()
    !   - get_node_type_kind()
    !   - get_node_type_details()
    
    ! Re-export core pipeline functionality
    use frontend, only: lex_source, parse_tokens, analyze_semantics, &
                       emit_fortran, &
                       transform_lazy_fortran_string, &
                       transform_lazy_fortran_string_with_format, &
                       compilation_options_t, format_options_t
    
    ! Re-export AST arena and core types
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node, call_or_subscript_node, &
                        subroutine_def_node, subroutine_call_node, declaration_node, &
                        parameter_declaration_node, if_node, do_loop_node, &
                        do_while_node, select_case_node, case_block_node, &
                        module_node, use_statement_node, include_statement_node, &
                        print_statement_node, write_statement_node, &
                        read_statement_node, format_descriptor_node, &
                        allocate_statement_node, deallocate_statement_node, &
                        stop_node, return_node, cycle_node, exit_node, &
                        where_node, interface_block_node, derived_type_node, &
                        pointer_assignment_node, forall_node, case_range_node, &
                        case_default_node, complex_literal_node, &
                        comment_node, contains_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX, &
                        create_ast_arena, ast_arena_stats_t
    
    ! Re-export AST node data utilities  
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                             INTENT_OUT, INTENT_INOUT
    
    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    
    ! Re-export lexer token type
    use lexer_core, only: token_t, tokenize_core
    
    ! Re-export type system
    use type_system_hm, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                             TFUN, TARRAY, TVAR
    
    ! Re-export scope management
    use scope_manager, only: scope_stack_t, SCOPE_GLOBAL, SCOPE_MODULE, &
                           SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK, &
                           SCOPE_INTERFACE
    
    ! Re-export AST introspection APIs for issue #12
    use ast_introspection, only: visit_node_at, get_node_type_id, has_semantic_info, &
                                get_node_source_location, &
                                get_node_type_kind, get_node_type_details, &
                                get_node_type_id_from_arena, get_node_source_location_from_arena
    
    ! Re-export AST traversal and visitor functionality
    use ast_traversal, only: traverse_ast_visitor => traverse_ast, &
                            traverse_preorder, traverse_postorder, &
                            is_program_node, is_assignment_node, is_binary_op_node, &
                            is_function_def_node, is_subroutine_def_node, &
                            is_identifier_node, is_literal_node, is_declaration_node, &
                            is_if_node, is_do_loop_node, is_do_while_node, &
                            is_call_or_subscript_node, is_subroutine_call_node, &
                            is_print_statement_node, is_use_statement_node, &
                            is_select_case_node, is_derived_type_node, &
                            is_module_node, is_interface_block_node
    
    ! Re-export visitor pattern
    use ast_visitor, only: ast_visitor_t, debug_visitor_t
    
    ! Re-export intrinsic function registry (using renamed imports to avoid conflicts)
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                 registry_get_signature => get_intrinsic_signature, &
                                 get_intrinsic_info, &
                                 initialize_intrinsic_registry, &
                                 intrinsic_signature_t
    
    implicit none
    public
    
    ! ===== SYMBOL TABLE AND SCOPE API TYPES =====
    
    ! Symbol reference information for cross-reference analysis
    type :: symbol_reference_t
        integer :: node_index = 0        ! Where symbol is referenced
        integer :: scope_level = 0       ! Scope level of reference
        logical :: is_definition = .false. ! True if this is the declaration
        logical :: is_assignment = .false. ! True if symbol is being assigned
    end type symbol_reference_t
    
    ! Scope information type
    type :: scope_info_t
        integer :: level = 0
        integer :: scope_type = 0  ! SCOPE_GLOBAL, SCOPE_MODULE, etc.
        character(len=:), allocatable :: name
        integer :: symbol_count = 0
    end type scope_info_t
    
    ! Public symbol table interface functions
    public :: symbol_info_t, symbol_reference_t, scope_info_t
    public :: get_symbol_info, get_symbols_in_scope, get_symbol_references, &
              get_scope_info, get_all_scopes
    
    ! Public node accessor functions
    public :: get_assignment_indices, get_binary_op_info, get_identifier_name, &
              get_literal_value, get_call_info, get_array_literal_info, &
              get_program_info, get_declaration_info, get_parameter_declaration_info, &
              get_declaration_details, get_parameter_declaration_details
    
    ! Public AST introspection APIs for issue #12
    public :: visit_node_at, get_node_type_id, get_node_source_location, &
              has_semantic_info, &
              get_node_type_kind, get_node_type_details, &
              get_node_type_id_from_arena, get_node_source_location_from_arena
    ! Node type constants for type queries
    integer, parameter :: NODE_PROGRAM = 1
    integer, parameter :: NODE_FUNCTION_DEF = 2
    integer, parameter :: NODE_ASSIGNMENT = 3
    integer, parameter :: NODE_BINARY_OP = 4
    integer, parameter :: NODE_IDENTIFIER = 5
    integer, parameter :: NODE_LITERAL = 6
    integer, parameter :: NODE_ARRAY_LITERAL = 7
    integer, parameter :: NODE_CALL_OR_SUBSCRIPT = 8
    integer, parameter :: NODE_SUBROUTINE_DEF = 9
    integer, parameter :: NODE_SUBROUTINE_CALL = 10
    integer, parameter :: NODE_DECLARATION = 11
    integer, parameter :: NODE_PARAMETER_DECLARATION = 12
    integer, parameter :: NODE_IF = 13
    integer, parameter :: NODE_DO_LOOP = 14
    integer, parameter :: NODE_DO_WHILE = 15
    integer, parameter :: NODE_SELECT_CASE = 16
    integer, parameter :: NODE_CASE_BLOCK = 17
    integer, parameter :: NODE_MODULE = 18
    integer, parameter :: NODE_USE_STATEMENT = 19
    integer, parameter :: NODE_PRINT_STATEMENT = 20
    integer, parameter :: NODE_WRITE_STATEMENT = 21
    integer, parameter :: NODE_READ_STATEMENT = 22
    integer, parameter :: NODE_ALLOCATE_STATEMENT = 23
    integer, parameter :: NODE_DEALLOCATE_STATEMENT = 24
    integer, parameter :: NODE_STOP = 25
    integer, parameter :: NODE_RETURN = 26
    integer, parameter :: NODE_CYCLE = 27
    integer, parameter :: NODE_EXIT = 28
    integer, parameter :: NODE_WHERE = 29
    integer, parameter :: NODE_INTERFACE_BLOCK = 30
    integer, parameter :: NODE_DERIVED_TYPE = 31
    integer, parameter :: NODE_POINTER_ASSIGNMENT = 32
    integer, parameter :: NODE_FORALL = 33
    integer, parameter :: NODE_CASE_RANGE = 34
    integer, parameter :: NODE_CASE_DEFAULT = 35
    integer, parameter :: NODE_COMPLEX_LITERAL = 36
    integer, parameter :: NODE_INCLUDE_STATEMENT = 37
    integer, parameter :: NODE_CONTAINS = 38
    integer, parameter :: NODE_FORMAT_DESCRIPTOR = 39
    integer, parameter :: NODE_UNKNOWN = 99
    
    ! Additional facade-specific types and procedures
    
    ! Source location tracking
    type :: source_location_t
        integer :: line = 1
        integer :: column = 1
        integer :: byte_offset = 0  ! For efficient text manipulation
    end type source_location_t
    
    type :: source_range_t
        type(source_location_t) :: start
        type(source_location_t) :: end
    end type source_range_t
    
    ! Diagnostic information
    integer, parameter :: DIAGNOSTIC_ERROR = 1
    integer, parameter :: DIAGNOSTIC_WARNING = 2
    integer, parameter :: DIAGNOSTIC_INFO = 3
    integer, parameter :: DIAGNOSTIC_HINT = 4
    
    ! Type information
    type :: type_info_t
        integer :: base_type = TVAR       ! TINT, TREAL, etc.
        integer :: bit_width = 32         ! 32, 64, etc.
        logical :: is_signed = .true.     ! For integers
        integer :: array_rank = 0         ! 0 for scalars
        integer, allocatable :: array_dims(:)  ! Shape if known
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        character(len=:), allocatable :: derived_type_name
    end type type_info_t
    
    type :: diagnostic_t
        integer :: severity = DIAGNOSTIC_INFO
        character(len=:), allocatable :: message
        type(source_range_t) :: location
        character(len=:), allocatable :: code      ! Error code (e.g., "F001")
        character(len=:), allocatable :: category  ! Error category
    end type diagnostic_t
    
    ! Enhanced symbol information type for static analysis
    type :: symbol_info_t
        character(len=:), allocatable :: name
        integer :: declaration_node_index = 0
        integer :: scope_level = 0
        character(len=:), allocatable :: scope_name
        type(mono_type_t) :: symbol_type
        logical :: is_parameter = .false.
        logical :: is_optional = .false.
        logical :: is_function = .false.
        logical :: is_subroutine = .false.
        logical :: is_module_variable = .false.
        integer :: intent_type = 0  ! 0=none, 1=in, 2=out, 3=inout
    end type symbol_info_t
    
    ! Function signature type
    type :: function_signature_t
        type(type_info_t), allocatable :: param_types(:)
        type(type_info_t) :: return_type
        logical :: is_elemental = .false.
        logical :: is_pure = .false.
    end type function_signature_t
    
contains
    
    ! Check if a node exists at the given index
    function node_exists(arena, node_index) result(exists)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: exists
        
        exists = node_index > 0 .and. node_index <= arena%size
        if (exists) then
            exists = allocated(arena%entries(node_index)%node)
        end if
    end function node_exists
    
    ! Get node type at index (returns empty string if invalid)
    function get_node_type_at(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: node_type
        
        if (node_exists(arena, node_index)) then
            node_type = arena%entries(node_index)%node_type
        else
            node_type = ""
        end if
    end function get_node_type_at
    
    ! Get node line/column info
    subroutine get_node_location(arena, node_index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: line, column
        
        line = 0
        column = 0
        if (node_exists(arena, node_index)) then
            line = arena%entries(node_index)%node%line
            column = arena%entries(node_index)%node%column
        end if
    end subroutine get_node_location
    
    
    ! Get parent node for a given node index
    function get_parent(arena, node_index) result(parent_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: parent_index
        
        parent_index = 0
        if (node_index > 0 .and. node_index <= arena%size) then
            parent_index = arena%entries(node_index)%parent_index
        end if
    end function get_parent
    
    ! Get children indices for a node
    function get_children(arena, node_index) result(child_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: child_indices(:)
        
        allocate(child_indices(0))
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%child_indices)) then
                child_indices = arena%entries(node_index)%child_indices(&
                    1:arena%entries(node_index)%child_count)
            end if
        end if
    end function get_children
    
    ! Traverse AST with callback procedure
    subroutine traverse_ast(arena, root_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in), optional :: pre_order
        
        logical :: do_pre_order
        
        do_pre_order = .true.
        if (present(pre_order)) do_pre_order = pre_order
        
        call traverse_node(arena, root_index, callback, do_pre_order)
    end subroutine traverse_ast
    
    ! Internal recursive traversal
    recursive subroutine traverse_node(arena, node_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in) :: pre_order
        
        integer, allocatable :: child_indices(:)
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Pre-order visit
        if (pre_order) then
            call callback(arena, node_index)
        end if
        
        ! Visit children
        child_indices = get_children(arena, node_index)
        do i = 1, size(child_indices)
            call traverse_node(arena, child_indices(i), callback, pre_order)
        end do
        
        ! Post-order visit
        if (.not. pre_order) then
            call callback(arena, node_index)
        end if
    end subroutine traverse_node
    
    ! Get source range for a node
    function get_node_range(arena, node_index) result(range)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(source_range_t) :: range
        
        integer :: line, column
        
        ! Initialize with default values
        range%start%line = 1
        range%start%column = 1
        range%start%byte_offset = 0
        range%end = range%start
        
        if (node_exists(arena, node_index)) then
            call get_node_location(arena, node_index, line, column)
            range%start%line = line
            range%start%column = column
            ! End position would need to be calculated based on node content
            ! For now, use same as start
            range%end = range%start
        end if
    end function get_node_range
    
    ! Arena statistics wrapper
    function get_arena_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(ast_arena_stats_t) :: stats
        
        ! Direct access instead of type-bound procedure to avoid compiler crash
        stats%total_nodes = arena%size
        stats%max_depth = arena%max_depth
        stats%capacity = arena%capacity
        stats%memory_usage = arena%capacity * 64  ! Rough estimate
    end function get_arena_stats
    
    ! Analyze program with explicit context (for advanced usage)
    subroutine analyze_program(ctx, arena, prog_index)
        use semantic_analyzer, only: analyze_program_impl => analyze_program
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        
        call analyze_program_impl(ctx, arena, prog_index)
    end subroutine analyze_program
    
    ! Get type information for a node
    subroutine get_type_for_node(arena, node_index, node_type, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t), allocatable, intent(out) :: node_type
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        if (node_exists(arena, node_index)) then
            if (allocated(arena%entries(node_index)%node)) then
                if (allocated(arena%entries(node_index)%node%inferred_type)) then
                    allocate(node_type)
                    ! Manual deep copy to avoid issues with assignment operator
                    associate (src_type => arena%entries(node_index)%node%inferred_type)
                        node_type%kind = src_type%kind
                        node_type%size = src_type%size
                        node_type%var%id = src_type%var%id
                        if (allocated(src_type%var%name)) then
                            node_type%var%name = src_type%var%name
                        else
                            allocate(character(len=0) :: node_type%var%name)
                        end if
                        if (allocated(src_type%args)) then
                            allocate(node_type%args(size(src_type%args)))
                            do i = 1, size(src_type%args)
                                ! For now, shallow copy args to avoid recursion issues
                                node_type%args(i) = src_type%args(i)
                            end do
                        end if
                    end associate
                    found = .true.
                end if
            end if
        end if
    end subroutine get_type_for_node
    
    ! Collect diagnostics (placeholder for future implementation)
    function get_diagnostics(ctx) result(diagnostics)
        type(semantic_context_t), intent(in) :: ctx
        type(diagnostic_t), allocatable :: diagnostics(:)
        
        ! Placeholder - return empty array
        allocate(diagnostics(0))
    end function get_diagnostics
    
    ! AST to JSON serialization
    subroutine ast_to_json(arena, root_index, json_string)
        use ast_json, only: ast_to_json_string
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable, intent(out) :: json_string
        
        json_string = ast_to_json_string(arena, root_index)
    end subroutine ast_to_json
    
    ! Semantic info to JSON (placeholder)
    subroutine semantic_info_to_json(ctx, json_string)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable, intent(out) :: json_string
        
        ! Placeholder implementation
        json_string = '{"semantic_info": "not implemented"}'
    end subroutine semantic_info_to_json
    
    ! Find nodes by type name
    function find_nodes_by_type(arena, type_name) result(indices)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name
        integer, allocatable :: indices(:)
        
        indices = arena%find_by_type(type_name)
    end function find_nodes_by_type
    
    ! Get maximum depth of AST from a given node
    function get_max_depth(arena, node_index) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: max_depth
        
        max_depth = compute_depth(arena, node_index, 1)  ! Start at depth 1 for root
    end function get_max_depth
    
    ! Recursively compute depth
    recursive function compute_depth(arena, node_index, current_depth) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: current_depth
        integer :: max_depth
        integer, allocatable :: children(:)
        integer :: i, child_depth
        
        max_depth = current_depth
        
        if (node_index <= 0 .or. node_index > arena%size) return
        
        children = get_children(arena, node_index)
        if (allocated(children)) then
            do i = 1, size(children)
                child_depth = compute_depth(arena, children(i), current_depth + 1)
                if (child_depth > max_depth) max_depth = child_depth
            end do
        end if
    end function compute_depth
    
    ! Get node type as integer constant
    function get_node_type(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: node_type
        class(ast_node), allocatable :: node
        
        node_type = NODE_UNKNOWN
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Map node type string to constant (handle both with and without _node suffix)
        select case (trim(arena%entries(node_index)%node_type))
        case ("program_node", "program")
            node_type = NODE_PROGRAM
        case ("function_def_node", "function_def")
            node_type = NODE_FUNCTION_DEF
        case ("assignment_node", "assignment")
            node_type = NODE_ASSIGNMENT
        case ("binary_op_node", "binary_op")
            node_type = NODE_BINARY_OP
        case ("identifier_node", "identifier")
            node_type = NODE_IDENTIFIER
        case ("literal_node", "literal")
            node_type = NODE_LITERAL
        case ("array_literal_node", "array_literal")
            node_type = NODE_ARRAY_LITERAL
        case ("call_or_subscript_node", "call_or_subscript")
            node_type = NODE_CALL_OR_SUBSCRIPT
        case ("subroutine_def_node", "subroutine_def")
            node_type = NODE_SUBROUTINE_DEF
        case ("subroutine_call_node", "subroutine_call")
            node_type = NODE_SUBROUTINE_CALL
        case ("declaration_node", "declaration")
            node_type = NODE_DECLARATION
        case ("parameter_declaration_node", "parameter_declaration")
            node_type = NODE_PARAMETER_DECLARATION
        case ("if_node", "if")
            node_type = NODE_IF
        case ("do_loop_node", "do_loop")
            node_type = NODE_DO_LOOP
        case ("do_while_node", "do_while")
            node_type = NODE_DO_WHILE
        case ("select_case_node", "select_case")
            node_type = NODE_SELECT_CASE
        case ("case_block_node", "case_block")
            node_type = NODE_CASE_BLOCK
        case ("module_node", "module")
            node_type = NODE_MODULE
        case ("use_statement_node", "use_statement")
            node_type = NODE_USE_STATEMENT
        case ("print_statement_node", "print_statement")
            node_type = NODE_PRINT_STATEMENT
        case ("write_statement_node", "write_statement")
            node_type = NODE_WRITE_STATEMENT
        case ("read_statement_node", "read_statement")
            node_type = NODE_READ_STATEMENT
        case ("allocate_statement_node", "allocate_statement")
            node_type = NODE_ALLOCATE_STATEMENT
        case ("deallocate_statement_node", "deallocate_statement")
            node_type = NODE_DEALLOCATE_STATEMENT
        case ("stop_node", "stop")
            node_type = NODE_STOP
        case ("return_node", "return")
            node_type = NODE_RETURN
        case ("cycle_node", "cycle")
            node_type = NODE_CYCLE
        case ("exit_node", "exit")
            node_type = NODE_EXIT
        case ("where_node", "where")
            node_type = NODE_WHERE
        case ("interface_block_node", "interface_block")
            node_type = NODE_INTERFACE_BLOCK
        case ("derived_type_node", "derived_type")
            node_type = NODE_DERIVED_TYPE
        case ("pointer_assignment_node", "pointer_assignment")
            node_type = NODE_POINTER_ASSIGNMENT
        case ("forall_node", "forall")
            node_type = NODE_FORALL
        case ("case_range_node", "case_range")
            node_type = NODE_CASE_RANGE
        case ("case_default_node", "case_default")
            node_type = NODE_CASE_DEFAULT
        case ("complex_literal_node", "complex_literal")
            node_type = NODE_COMPLEX_LITERAL
        case ("include_statement_node", "include_statement")
            node_type = NODE_INCLUDE_STATEMENT
        case ("contains_node", "contains")
            node_type = NODE_CONTAINS
        case ("format_descriptor_node", "format_descriptor")
            node_type = NODE_FORMAT_DESCRIPTOR
        end select
    end function get_node_type
    
    ! Get typed node access functions
    function get_node_as_program(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(program_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (program_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_program
    
    function get_node_as_assignment(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(assignment_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (assignment_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_assignment
    
    function get_node_as_function_def(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(function_def_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (function_def_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_function_def
    
    ! ===== TYPE-SAFE ACCESSOR FUNCTIONS =====
    ! These functions provide safe access to node fields without exposing &
    ! internal structure
    
    ! Assignment node accessors
    function get_assignment_indices(arena, node_index, target_index, &
                                     value_index, operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: target_index, value_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        target_index = 0
        value_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (assignment_node)
                    target_index = node%target_index
                    value_index = node%value_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "="
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_assignment_indices
    
    ! Binary operation accessors
    function get_binary_op_info(arena, node_index, left_index, right_index, &
                                 operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: left_index, right_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        left_index = 0
        right_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (binary_op_node)
                    left_index = node%left_index
                    right_index = node%right_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_binary_op_info
    
    ! Identifier accessors
    function get_identifier_name(arena, node_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        logical :: found
        
        found = .false.
        ! name is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (identifier_node)
                    if (allocated(node%name)) then
                        name = node%name
                        found = .true.
                    end if
                end select
            end if
        end if
    end function get_identifier_name
    
    ! Literal accessors
    function get_literal_value(arena, node_index, value, literal_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: value, literal_type
        logical :: found
        
        found = .false.
        ! value and literal_type are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (allocated(node%value)) then
                        value = node%value
                    else
                        value = ""
                    end if
                    if (allocated(node%literal_type)) then
                        literal_type = node%literal_type
                    else
                        literal_type = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_literal_value
    
    ! Function/array call accessors
    function get_call_info(arena, node_index, name, arg_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: arg_indices(:)
        logical :: found
        
        found = .false.
        ! name and arg_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    if (allocated(node%arg_indices)) then
                        allocate(arg_indices(size(node%arg_indices)))
                        arg_indices = node%arg_indices
                    else
                        allocate(arg_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_call_info
    
    ! Array literal accessors
    function get_array_literal_info(arena, node_index, element_indices, &
                                     element_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: element_indices(:)
        character(len=:), allocatable, intent(out) :: element_type
        logical :: found
        
        found = .false.
        ! element_indices and element_type are intent(out) - automatically &
        ! deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices)) then
                        allocate(element_indices(size(node%element_indices)))
                        element_indices = node%element_indices
                    else
                        allocate(element_indices(0))
                    end if
                    if (allocated(node%element_type)) then
                        element_type = node%element_type
                    else
                        element_type = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_array_literal_info
    
    ! Program node accessors
    function get_program_info(arena, node_index, name, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        logical :: found
        
        found = .false.
        ! name and body_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    if (allocated(node%body_indices)) then
                        allocate(body_indices(size(node%body_indices)))
                        body_indices = node%body_indices
                    else
                        allocate(body_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_program_info
    
    ! Comprehensive declaration node accessor - provides access to ALL &
    ! declaration details
    function get_declaration_info(arena, node_index, var_names, type_spec, &
                                   attributes) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        logical :: found
        
        found = .false.
        ! All output parameters are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    ! Get variable names
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        allocate(character(len=len(node%var_names)) :: &
                            var_names(size(node%var_names)))
                        var_names = node%var_names
                    else if (allocated(node%var_name)) then
                        allocate(character(len=len(node%var_name)) :: var_names(1))
                        var_names(1) = node%var_name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Get comprehensive type specification with kind 
                    if (allocated(node%type_name)) then
                        if (node%has_kind) then
                            type_spec = node%type_name // "(" // trim(adjustl(int_to_str(node%kind_value))) // ")"
                        else
                            type_spec = node%type_name
                        end if
                    else
                        type_spec = "unknown"
                    end if
                    
                    ! Build comprehensive attributes list
                    call build_declaration_attributes(node, attributes)
                    
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_info

    ! Get detailed declaration information with all fields accessible
    function get_declaration_details(arena, node_index, var_names, type_name, kind_value, has_kind, &
                                   intent_spec, has_intent, is_array, dimension_indices, &
                                   is_allocatable, is_pointer, initializer_index, has_initializer) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_name
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        character(len=:), allocatable, intent(out) :: intent_spec
        logical, intent(out) :: has_intent
        logical, intent(out) :: is_array
        integer, allocatable, intent(out) :: dimension_indices(:)
        logical, intent(out) :: is_allocatable
        logical, intent(out) :: is_pointer
        integer, intent(out) :: initializer_index
        logical, intent(out) :: has_initializer
        logical :: found
        
        found = .false.
        ! Initialize output parameters
        kind_value = 0
        has_kind = .false.
        has_intent = .false.
        is_array = .false.
        is_allocatable = .false.
        is_pointer = .false.
        initializer_index = 0
        has_initializer = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    ! Variable names
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        allocate(character(len=len(node%var_names)) :: &
                            var_names(size(node%var_names)))
                        var_names = node%var_names
                    else if (allocated(node%var_name)) then
                        allocate(character(len=len(node%var_name)) :: var_names(1))
                        var_names(1) = node%var_name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Type information
                    if (allocated(node%type_name)) then
                        type_name = node%type_name
                    else
                        type_name = "unknown"
                    end if
                    
                    ! Kind information
                    kind_value = node%kind_value
                    has_kind = node%has_kind
                    
                    ! Intent information
                    if (allocated(node%intent)) then
                        intent_spec = node%intent
                    else
                        intent_spec = ""
                    end if
                    has_intent = node%has_intent
                    
                    ! Array information
                    is_array = node%is_array
                    if (allocated(node%dimension_indices)) then
                        allocate(dimension_indices(size(node%dimension_indices)))
                        dimension_indices = node%dimension_indices
                    else
                        allocate(dimension_indices(0))
                    end if
                    
                    ! Memory attributes
                    is_allocatable = node%is_allocatable
                    is_pointer = node%is_pointer
                    
                    ! Initializer information
                    initializer_index = node%initializer_index 
                    has_initializer = node%has_initializer
                    
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_details
    
    ! Comprehensive parameter declaration node accessor
    function get_parameter_declaration_info(arena, node_index, var_names, values, type_spec) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: values(:)
        character(len=:), allocatable, intent(out) :: type_spec
        logical :: found
        
        found = .false.
        ! All output parameters are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Get parameter names
                    if (allocated(node%name)) then
                        allocate(character(len=len(node%name)) :: var_names(1))
                        var_names(1) = node%name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Parameter values - return empty for now (future: could access initializers)
                    allocate(character(len=1) :: values(0))
                    
                    ! Get comprehensive type specification with kind
                    if (allocated(node%type_name)) then
                        if (node%has_kind) then
                            type_spec = node%type_name // "(" // trim(adjustl(int_to_str(node%kind_value))) // ")"
                        else
                            type_spec = node%type_name
                        end if
                    else
                        type_spec = "unknown"
                    end if
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_info

    ! Get detailed parameter declaration information with all fields accessible  
    function get_parameter_declaration_details(arena, node_index, name, type_name, kind_value, has_kind, &
                                             intent_spec, has_intent, is_array, dimension_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: type_name
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        character(len=:), allocatable, intent(out) :: intent_spec
        logical, intent(out) :: has_intent
        logical, intent(out) :: is_array
        integer, allocatable, intent(out) :: dimension_indices(:)
        logical :: found
        
        found = .false.
        ! Initialize output parameters
        kind_value = 0
        has_kind = .false.
        has_intent = .false.
        is_array = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Parameter name
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    
                    ! Type information
                    if (allocated(node%type_name)) then
                        type_name = node%type_name
                    else
                        type_name = "unknown"
                    end if
                    
                    ! Kind information
                    kind_value = node%kind_value
                    has_kind = node%has_kind
                    
                    ! Intent information
                    intent_spec = intent_type_to_string(node%intent_type)
                    has_intent = (node%intent_type /= INTENT_NONE)
                    
                    ! Array information
                    is_array = node%is_array
                    if (allocated(node%dimension_indices)) then
                        allocate(dimension_indices(size(node%dimension_indices)))
                        dimension_indices = node%dimension_indices
                    else
                        allocate(dimension_indices(0))
                    end if
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_details
    
    ! ===== SYMBOL TABLE AND SCOPE API IMPLEMENTATION =====
    
    ! Symbol lookup - enhanced version
    function lookup_symbol(ctx, name, scope_node_index) result(symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: scope_node_index
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        
        ! Initialize symbol with defaults
        symbol%name = ""
        symbol%declaration_node_index = 0
        symbol%scope_level = 0
        symbol%is_parameter = .false.
        symbol%is_module_variable = .false.
        symbol%is_function = .false.
        symbol%is_subroutine = .false.
        
        ! Try to find symbol in scopes
        call ctx%scopes%lookup(name, scheme)
        if (allocated(scheme)) then
            symbol%name = name
            symbol%symbol_type = scheme%mono
        end if
    end function lookup_symbol
    
    ! Get enhanced symbol information
    function get_symbol_info(ctx, name, scope_level) result(symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: scope_level  ! If not provided, searches all scopes
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        integer :: search_scope
        
        ! Initialize with defaults
        symbol%name = ""
        symbol%declaration_node_index = 0
        symbol%scope_level = 0
        symbol%scope_name = ""
        symbol%is_parameter = .false.
        symbol%is_optional = .false.
        symbol%is_function = .false.
        symbol%is_subroutine = .false.
        symbol%is_module_variable = .false.
        symbol%intent_type = 0
        
        ! Search for symbol in scopes
        if (present(scope_level)) then
            search_scope = scope_level
        else
            search_scope = -1  ! Search all scopes
        end if
        
        call ctx%scopes%lookup(name, scheme)
        if (allocated(scheme)) then
            symbol%name = name
            symbol%symbol_type = scheme%mono
            symbol%scope_level = ctx%scopes%depth
            
            ! Get scope name if available
            if (ctx%scopes%depth > 0) then
                symbol%scope_name = ctx%scopes%scopes(ctx%scopes%depth)%name
            else
                symbol%scope_name = "global"
            end if
            
            ! Check parameter tracker for additional attributes
            if (ctx%param_tracker%count > 0) then
                call check_parameter_attributes(ctx, name, symbol)
            end if
        end if
    end function get_symbol_info
    
    ! Get all symbols in a specific scope level
    function get_symbols_in_scope(ctx, scope_level) result(symbols)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_level
        type(symbol_info_t), allocatable :: symbols(:)
        integer :: i, count, symbol_idx
        
        ! Count symbols in the specified scope
        count = 0
        if (scope_level > 0 .and. scope_level <= ctx%scopes%depth) then
            count = ctx%scopes%scopes(scope_level)%env%count
        end if
        
        ! Allocate result array
        allocate(symbols(count))
        
        ! Populate symbols
        if (count > 0) then
            do i = 1, count
                symbols(i)%name = ctx%scopes%scopes(scope_level)%env%names(i)
                symbols(i)%symbol_type = ctx%scopes%scopes(scope_level)%env%schemes(i)%mono
                symbols(i)%scope_level = scope_level
                symbols(i)%scope_name = ctx%scopes%scopes(scope_level)%name
                
                ! Check parameter attributes
                call check_parameter_attributes(ctx, symbols(i)%name, symbols(i))
            end do
        end if
    end function get_symbols_in_scope
    
    ! Get references to a symbol (for cross-reference analysis) - optimized single pass
    function get_symbol_references(arena, ctx, symbol_name) result(references)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: symbol_name
        type(symbol_reference_t), allocatable :: references(:)
        integer :: i, ref_count, initial_capacity
        character(len=:), allocatable :: node_name
        
        ! Pre-allocate with reasonable initial capacity to minimize reallocations
        initial_capacity = min(64, max(1, arena%size / 10))  
        allocate(references(initial_capacity))
        ref_count = 0
        
        ! Single pass: find and collect references
        do i = 1, arena%size
            if (get_identifier_name(arena, i, node_name)) then
                if (node_name == symbol_name) then
                    ref_count = ref_count + 1
                    
                    ! Grow array if needed using geometric growth
                    if (ref_count > size(references)) then
                        references = [references, references]  ! Double the size
                    end if
                    
                    ! Populate reference information
                    references(ref_count)%node_index = i
                    references(ref_count)%scope_level = ctx%scopes%depth
                    references(ref_count)%is_definition = .false.  ! Would need AST analysis to determine
                    references(ref_count)%is_assignment = .false.  ! Would need parent node analysis
                end if
            end if
        end do
        
        ! Trim array to actual size to free unused memory
        if (ref_count < size(references)) then
            references = references(1:ref_count)
        end if
    end function get_symbol_references
    
    ! Get scope information
    function get_scope_info(ctx, scope_level) result(scope_info)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_level
        type(scope_info_t) :: scope_info
        
        scope_info%level = 0
        scope_info%scope_type = 0
        scope_info%name = ""
        scope_info%symbol_count = 0
        
        if (scope_level > 0 .and. scope_level <= ctx%scopes%depth) then
            scope_info%level = scope_level
            scope_info%scope_type = ctx%scopes%scopes(scope_level)%scope_type
            scope_info%name = ctx%scopes%scopes(scope_level)%name
            scope_info%symbol_count = ctx%scopes%scopes(scope_level)%env%count
        end if
    end function get_scope_info
    
    ! Get all scope information
    function get_all_scopes(ctx) result(scopes)
        type(semantic_context_t), intent(in) :: ctx
        type(scope_info_t), allocatable :: scopes(:)
        integer :: i
        
        allocate(scopes(ctx%scopes%depth))
        
        do i = 1, ctx%scopes%depth
            scopes(i) = get_scope_info(ctx, i)
        end do
    end function get_all_scopes
    
    ! Helper to check parameter attributes
    subroutine check_parameter_attributes(ctx, name, symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        type(symbol_info_t), intent(inout) :: symbol
        integer :: i
        
        ! Check parameter tracker for attributes
        do i = 1, ctx%param_tracker%count
            if (ctx%param_tracker%params(i)%name == name) then
                symbol%is_parameter = .true.
                symbol%is_optional = ctx%param_tracker%params(i)%is_optional
                ! Convert intent string to integer
                if (ctx%param_tracker%params(i)%intent == "in") then
                    symbol%intent_type = 1
                else if (ctx%param_tracker%params(i)%intent == "out") then
                    symbol%intent_type = 2
                else if (ctx%param_tracker%params(i)%intent == "inout") then
                    symbol%intent_type = 3
                else
                    symbol%intent_type = 0
                end if
                exit
            end if
        end do
    end subroutine check_parameter_attributes
    
    ! Get all symbols in a scope
    function get_scope_symbols(ctx, scope_node_index) result(symbols)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_node_index
        type(symbol_info_t), allocatable :: symbols(:)
        
        ! For now, return empty array as we don't have a way to enumerate all symbols
        ! This would require extending the scope_manager module
        allocate(symbols(0))
        
        ! TODO: Implement proper symbol enumeration when scope_manager supports it
    end function get_scope_symbols
    
    ! Convert mono_type to type_info
    recursive function mono_type_to_type_info(mono) result(info)
        type(mono_type_t), intent(in) :: mono
        type(type_info_t) :: info
        
        info%base_type = mono%kind
        
        select case (mono%kind)
        case (TINT)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TREAL)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TCHAR)
            info%bit_width = 8
            info%is_signed = .false.
            info%array_rank = 0
        case (TLOGICAL)
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        case (TARRAY)
            if (allocated(mono%args)) then
                if (size(mono%args) > 0) then
                    info = mono_type_to_type_info(mono%args(1))  ! Element type
                    info%array_rank = 1  ! Simple 1D array for now
                end if
            end if
        end select
    end function mono_type_to_type_info
    
    ! Get type info for base type
    function get_type_info_for_base_type(base_type) result(info)
        integer, intent(in) :: base_type
        type(type_info_t) :: info
        
        info%base_type = base_type
        
        select case (base_type)
        case (TINT)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TREAL)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TCHAR)
            info%bit_width = 8
            info%is_signed = .false.
            info%array_rank = 0
        case (TLOGICAL)
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        case default
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        end select
    end function get_type_info_for_base_type
    
    ! Get integer literal value
    function get_integer_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer(kind=8) :: value
        class(ast_node), allocatable :: node
        integer :: i
        
        value = 0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_INTEGER) then
                        read(p%value, *, iostat=i) value
                    end if
                end select
            end if
        end if
    end function get_integer_literal_value
    
    ! Get real literal value
    function get_real_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        real(kind=8) :: value
        class(ast_node), allocatable :: node
        integer :: i
        
        value = 0.0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_REAL) then
                        read(p%value, *, iostat=i) value
                    end if
                end select
            end if
        end if
    end function get_real_literal_value
    
    ! Get string literal value
    function get_string_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: value
        class(ast_node), allocatable :: node
        
        value = ""
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_STRING) then
                        ! Remove quotes if present
                        if (len(p%value) >= 2) then
                            if ((p%value(1:1) == '"' .and. &
                                 p%value(len(p%value):len(p%value)) == '"') .or. &
                                (p%value(1:1) == "'" .and. &
                                 p%value(len(p%value):len(p%value)) == "'")) then
                                value = p%value(2:len(p%value)-1)
                            else
                                value = p%value
                            end if
                        else
                            value = p%value
                        end if
                    end if
                end select
            end if
        end if
    end function get_string_literal_value
    
    ! Check if function is intrinsic
    function is_intrinsic_function(name) result(is_intrinsic)
        character(len=*), intent(in) :: name
        logical :: is_intrinsic
        
        ! Common Fortran intrinsic functions
        select case (trim(adjustl(name)))
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
              "exp", "log", "log10", "sqrt", "abs", &
              "max", "min", "mod", "sign", &
              "int", "real", "dble", "cmplx", &
              "len", "len_trim", "trim", "adjustl", "adjustr", &
              "index", "scan", "verify", &
              "allocated", "associated", "present", &
              "size", "shape", "lbound", "ubound", &
              "merge", "pack", "unpack", "reshape", &
              "matmul", "dot_product", "transpose", &
              "sum", "product", "maxval", "minval", &
              "count", "any", "all")
            is_intrinsic = .true.
        case default
            is_intrinsic = .false.
        end select
    end function is_intrinsic_function
    
    ! Get intrinsic function signature
    function get_intrinsic_signature(name) result(signature)
        character(len=*), intent(in) :: name
        type(function_signature_t) :: signature
        
        ! Initialize with defaults
        signature%is_elemental = .false.
        signature%is_pure = .true.
        
        ! Simple examples for common intrinsics
        select case (trim(adjustl(name)))
        case ("sin", "cos", "tan", "exp", "log", "sqrt", "abs")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TREAL)
            signature%return_type = get_type_info_for_base_type(TREAL)
            signature%is_elemental = .true.
        case ("int")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TREAL)
            signature%return_type = get_type_info_for_base_type(TINT)
            signature%is_elemental = .true.
        case ("real", "dble")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TINT)
            signature%return_type = get_type_info_for_base_type(TREAL)
            signature%is_elemental = .true.
        case ("len", "len_trim")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TCHAR)
            signature%return_type = get_type_info_for_base_type(TINT)
        case default
            ! Unknown intrinsic - return empty signature
        end select
    end function get_intrinsic_signature
    
    ! Get string representation of type
    function get_type_string(type_info) result(type_str)
        type(type_info_t), intent(in) :: type_info
        character(len=:), allocatable :: type_str
        character(len=10) :: bit_str
        
        select case (type_info%base_type)
        case (TINT)
            write(bit_str, '(I0)') type_info%bit_width / 8
            type_str = "integer(" // trim(bit_str) // ")"
        case (TREAL)
            write(bit_str, '(I0)') type_info%bit_width / 8
            type_str = "real(" // trim(bit_str) // ")"
        case (TCHAR)
            type_str = "character"
        case (TLOGICAL)
            type_str = "logical"
        case (TARRAY)
            type_str = "array"
            if (type_info%array_rank > 0) then
                write(bit_str, '(I0)') type_info%array_rank
                type_str = type_str // "(" // trim(bit_str) // "D)"
            end if
        case (TFUN)
            type_str = "function"
        case (TVAR)
            type_str = "type_variable"
        case default
            type_str = "unknown"
        end select
        
        if (type_info%array_rank > 0 .and. type_info%base_type /= TARRAY) then
            type_str = type_str // ", dimension(:)"
        end if
    end function get_type_string
    
    ! ===== HELPER FUNCTIONS =====

    ! Convert integer to string
    function int_to_str(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=32) :: str
        
        write(str, '(I0)') int_val
        str = trim(adjustl(str))
    end function int_to_str

    ! Build comprehensive declaration attributes list
    subroutine build_declaration_attributes(node, attributes)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: attributes(:)
        character(len=:), allocatable :: temp_attrs(:)
        integer :: attr_count, i
        
        ! Count attributes
        attr_count = 0
        if (node%has_intent .and. allocated(node%intent)) attr_count = attr_count + 1
        if (node%is_array) attr_count = attr_count + 1
        if (node%is_allocatable) attr_count = attr_count + 1
        if (node%is_pointer) attr_count = attr_count + 1
        if (node%has_initializer) attr_count = attr_count + 1
        
        ! Allocate result array
        if (attr_count > 0) then
            allocate(character(len=64) :: attributes(attr_count))
        else
            allocate(character(len=1) :: attributes(0))
            return
        end if
        
        ! Build attributes list
        i = 0
        if (node%has_intent .and. allocated(node%intent)) then
            i = i + 1
            attributes(i) = "intent(" // node%intent // ")"
        end if
        if (node%is_array) then
            i = i + 1
            if (allocated(node%dimension_indices)) then
                attributes(i) = "dimension(" // trim(int_to_str(size(node%dimension_indices))) // ")"
            else
                attributes(i) = "dimension(:)"
            end if
        end if
        if (node%is_allocatable) then
            i = i + 1
            attributes(i) = "allocatable"
        end if
        if (node%is_pointer) then
            i = i + 1
            attributes(i) = "pointer"
        end if
        if (node%has_initializer) then
            i = i + 1
            attributes(i) = "initialized"
        end if
    end subroutine build_declaration_attributes

end module fortfront