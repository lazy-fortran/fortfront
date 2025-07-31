module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    
    ! Re-export core pipeline functionality
    use frontend, only: lex_source, parse_tokens, analyze_semantics, emit_fortran, &
                       transform_lazy_fortran_string, transform_lazy_fortran_string_with_format, &
                       compilation_options_t, format_options_t
    
    ! Re-export AST arena and core types
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node, call_or_subscript_node, &
                        subroutine_def_node, subroutine_call_node, declaration_node, &
                        parameter_declaration_node, if_node, do_loop_node, &
                        do_while_node, select_case_node, case_block_node, &
                        module_node, use_statement_node, include_statement_node, &
                        print_statement_node, write_statement_node, read_statement_node, &
                        allocate_statement_node, deallocate_statement_node, &
                        stop_node, return_node, cycle_node, exit_node, &
                        where_node, interface_block_node, derived_type_node, &
                        pointer_assignment_node, forall_node, case_range_node, &
                        case_default_node, complex_literal_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX, &
                        create_ast_arena, ast_arena_stats_t
    
    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    
    ! Re-export lexer token type
    use lexer_core, only: token_t, tokenize_core
    
    ! Re-export type system
    use type_system_hm, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                             TFUN, TARRAY, TVAR
    
    implicit none
    public
    
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
    
    ! Symbol information type
    type :: symbol_info_t
        character(len=:), allocatable :: name
        integer :: declaration_node_index = 0
        integer :: scope_level = 0
        type(type_info_t) :: type_info
        logical :: is_parameter = .false.
        logical :: is_module_variable = .false.
        logical :: is_function = .false.
        logical :: is_subroutine = .false.
    end type symbol_info_t
    
    ! Function signature type
    type :: function_signature_t
        type(type_info_t), allocatable :: param_types(:)
        type(type_info_t) :: return_type
        logical :: is_elemental = .false.
        logical :: is_pure = .false.
    end type function_signature_t
    
contains
    
    ! Get node from arena by index
    function get_node(arena, node_index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_node), allocatable :: node
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                allocate(node, mold=arena%entries(node_index)%node)
                ! Copy base fields manually
                node%line = arena%entries(node_index)%node%line
                node%column = arena%entries(node_index)%node%column
                ! Don't copy inferred_type to avoid double free issues
                ! The returned node is mainly for inspection, not modification
                ! Note: This only copies base ast_node fields. Specific node fields are not copied.
                ! This function should ideally not be used for copying nodes with complex structures.
            end if
        end if
    end function get_node
    
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
                child_indices = arena%entries(node_index)%child_indices(1:arena%entries(node_index)%child_count)
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
        
        class(ast_node), allocatable :: node
        
        ! Initialize with default values
        range%start%line = 1
        range%start%column = 1
        range%start%byte_offset = 0
        range%end = range%start
        
        node = get_node(arena, node_index)
        if (allocated(node)) then
            range%start%line = node%line
            range%start%column = node%column
            ! End position would need to be calculated based on node content
            ! For now, use same as start
            range%end = range%start
        end if
    end function get_node_range
    
    ! Arena statistics wrapper
    function get_arena_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(ast_arena_stats_t) :: stats
        
        stats = arena%get_stats()
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
        
        class(ast_node), allocatable :: node
        integer :: i
        
        found = .false.
        node = get_node(arena, node_index)
        if (allocated(node)) then
            if (allocated(node%inferred_type)) then
                allocate(node_type)
                ! Manual deep copy to avoid issues with assignment operator
                node_type%kind = node%inferred_type%kind
                node_type%size = node%inferred_type%size
                node_type%var%id = node%inferred_type%var%id
                if (allocated(node%inferred_type%var%name)) then
                    node_type%var%name = node%inferred_type%var%name
                else
                    allocate(character(len=0) :: node_type%var%name)
                end if
                if (allocated(node%inferred_type%args)) then
                    allocate(node_type%args(size(node%inferred_type%args)))
                    do i = 1, size(node%inferred_type%args)
                        ! For now, shallow copy args to avoid recursion issues
                        node_type%args(i) = node%inferred_type%args(i)
                    end do
                end if
                found = .true.
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
    
    ! Symbol lookup
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
            ! Convert poly_type to type_info
            symbol%type_info = mono_type_to_type_info(scheme%mono)
        end if
    end function lookup_symbol
    
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
                            if ((p%value(1:1) == '"' .and. p%value(len(p%value):len(p%value)) == '"') .or. &
                                (p%value(1:1) == "'" .and. p%value(len(p%value):len(p%value)) == "'")) then
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
    
end module fortfront