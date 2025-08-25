module cst_to_ast_converter
    ! CST to AST Converter Module
    ! ===========================
    ! Converts CST nodes to clean AST nodes while maintaining bidirectional linking.
    ! This is the final piece of the CST/AST split architecture.
    !
    ! Features:
    ! - Strip trivia for clean semantic analysis
    ! - Maintain UID-based bidirectional linking
    ! - Performance target: <50% of parsing time
    ! - Memory overhead: <50% additional memory
    
    use cst_nodes, only: cst_node_t, CST_PROGRAM, CST_SUBROUTINE, CST_FUNCTION, &
                        CST_DECLARATION, CST_ASSIGNMENT, CST_CALL, &
                        CST_IDENTIFIER, CST_LITERAL, CST_OPERATOR
    use cst_arena, only: cst_arena_t, cst_handle_t
    use ast_base, only: ast_node
    use ast_types, only: program_node, assignment_node, binary_op_node, &
                        call_or_subscript_node, subroutine_call_node, &
                        identifier_node, literal_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL, &
                        ast_arena_t, create_ast_arena
    use uid_generator, only: uid_t
    use string_types, only: string_t
    use error_handling, only: result_t, success_result, create_error_result, &
                             ERROR_INTERNAL, ERROR_VALIDATION
    implicit none
    private
    
    public :: cst_to_ast_converter_t
    public :: create_converter
    public :: conversion_result_t
    
    ! Conversion result type
    type :: conversion_result_t
        type(result_t) :: result                    ! Success/failure status
        integer :: ast_root_index = 0              ! Root AST node index
        integer :: nodes_converted = 0             ! Statistics
        integer :: trivia_stripped = 0             ! Statistics
    end type conversion_result_t
    
    ! CST to AST converter type
    type :: cst_to_ast_converter_t
        type(ast_arena_t) :: ast_arena             ! Target AST arena
        integer :: conversion_count = 0            ! Total conversions
        logical :: strip_trivia = .true.          ! Strip trivia (default true)
        logical :: preserve_positions = .true.     ! Preserve source positions
        logical :: validate_uids = .true.         ! Validate UID consistency
    contains
    end type cst_to_ast_converter_t
    
    ! Converter statistics type
    type :: converter_stats_t
        integer :: total_conversions = 0
        integer :: nodes_converted = 0
        integer :: trivia_entries_stripped = 0
        integer :: average_conversion_time_ms = 0
    end type converter_stats_t
    
contains
    
    ! Create new CST to AST converter
    function create_converter(initial_capacity) result(converter)
        integer, intent(in), optional :: initial_capacity
        type(cst_to_ast_converter_t) :: converter
        
        integer :: capacity
        
        capacity = 1024  ! Default capacity
        if (present(initial_capacity)) capacity = initial_capacity
        
        converter%ast_arena = create_ast_arena(capacity)
        converter%conversion_count = 0
        converter%strip_trivia = .true.
        converter%preserve_positions = .true.
        converter%validate_uids = .true.
    end function create_converter
    
    ! Convert entire CST tree to AST
    function convert_cst_tree_to_ast(this, cst_arena, root_handle) result(conv_result)
        class(cst_to_ast_converter_t), intent(inout) :: this
        type(cst_arena_t), intent(in) :: cst_arena
        type(cst_handle_t), intent(in) :: root_handle
        type(conversion_result_t) :: conv_result
        
        type(cst_node_t) :: root_cst
        integer :: ast_index
        
        ! Initialize result
        conv_result%result = success_result()
        conv_result%ast_root_index = 0
        conv_result%nodes_converted = 0
        conv_result%trivia_stripped = 0
        
        ! Validate root handle
        if (.not. cst_arena%is_valid_handle(root_handle)) then
            conv_result%result = create_error_result( &
                "Invalid CST root handle provided", &
                ERROR_VALIDATION, &
                component="cst_to_ast_converter", &
                context="convert_cst_tree_to_ast", &
                suggestion="Ensure CST root handle is valid before conversion" &
            )
            return
        end if
        
        ! Get root CST node
        root_cst = cst_arena%get(root_handle)
        if (root_cst%kind < 0) then
            conv_result%result = create_error_result( &
                "Invalid CST root node retrieved", &
                ERROR_VALIDATION, &
                component="cst_to_ast_converter", &
                context="convert_cst_tree_to_ast", &
                suggestion="Ensure CST tree is properly constructed" &
            )
            return
        end if
        
        ! Convert root node and all children recursively
        call convert_node_recursive(this, cst_arena, root_cst, 0, &
                                   ast_index, conv_result)
        
        if (conv_result%result%is_failure()) return
        
        conv_result%ast_root_index = ast_index
        this%conversion_count = this%conversion_count + 1
    end function convert_cst_tree_to_ast
    
    ! Convert single CST node to AST node (internal recursive helper)
    recursive subroutine convert_node_recursive(this, cst_arena, cst_node, &
                                               parent_index, ast_index, conv_result)
        class(cst_to_ast_converter_t), intent(inout) :: this
        type(cst_arena_t), intent(in) :: cst_arena
        type(cst_node_t), intent(in) :: cst_node
        integer, intent(in) :: parent_index
        integer, intent(out) :: ast_index
        type(conversion_result_t), intent(inout) :: conv_result
        
        class(ast_node), allocatable :: ast_node_obj
        integer :: child_ast_index, i
        type(cst_node_t) :: child_cst
        type(cst_handle_t) :: child_handle
        
        ! Convert CST node to appropriate AST node type
        call create_ast_node_from_cst(cst_node, ast_node_obj, conv_result)
        if (conv_result%result%is_failure()) return
        
        ! Count trivia stripped
        if (allocated(cst_node%leading_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%leading_trivia)
        end if
        if (allocated(cst_node%trailing_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%trailing_trivia)
        end if
        
        ! Set UID for bidirectional linking
        ast_node_obj%uid%value = cst_node%uid
        
        ! Preserve source positions if enabled
        if (this%preserve_positions) then
            ! Map CST positions to AST line/column (simplified mapping)
            ast_node_obj%line = max(1, cst_node%start_pos)
            ast_node_obj%column = 1  ! Simplified - would need lexer context for precise column
        end if
        
        ! Add to AST arena
        call this%ast_arena%push(ast_node_obj)
        ast_index = this%ast_arena%size  ! Use current size as index
        conv_result%nodes_converted = conv_result%nodes_converted + 1
        
        ! Recursively convert children
        if (allocated(cst_node%children)) then
            do i = 1, size(cst_node%children)
                ! Create child handle
                child_handle%index = cst_node%children(i)
                child_handle%generation = cst_arena%global_generation
                
                ! Get child CST node
                child_cst = cst_arena%get(child_handle)
                if (child_cst%kind < 0) cycle  ! Skip invalid children
                
                ! Recursively convert child
                call convert_node_recursive(this, cst_arena, child_cst, &
                                           ast_index, child_ast_index, conv_result)
                if (conv_result%result%is_failure()) return
                
                ! Note: Parent-child relationships are maintained through arena structure
            end do
        end if
    end subroutine convert_node_recursive
    
    ! Create appropriate AST node from CST node
    subroutine create_ast_node_from_cst(cst_node, ast_node_obj, conv_result)
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), allocatable, intent(out) :: ast_node_obj
        type(conversion_result_t), intent(inout) :: conv_result
        
        type(string_t) :: text_string
        
        select case (cst_node%kind)
        case (CST_PROGRAM)
            allocate(program_node :: ast_node_obj)
            
        case (CST_ASSIGNMENT)
            allocate(assignment_node :: ast_node_obj)
            
        case (CST_CALL)
            allocate(subroutine_call_node :: ast_node_obj)
            
        case (CST_IDENTIFIER)
            allocate(identifier_node :: ast_node_obj)
            if (allocated(cst_node%text)) then
                text_string = string_t(cst_node%text)
                select type (id_node => ast_node_obj)
                type is (identifier_node)
                    id_node%name = cst_node%text
                end select
            end if
            
        case (CST_LITERAL)
            allocate(literal_node :: ast_node_obj)
            if (allocated(cst_node%text)) then
                text_string = string_t(cst_node%text)
                select type (lit_node => ast_node_obj)
                type is (literal_node)
                    lit_node%value = cst_node%text
                    ! Infer literal type from text content
                    call infer_literal_type(cst_node%text, lit_node)
                end select
            end if
            
        case (CST_OPERATOR)
            allocate(binary_op_node :: ast_node_obj)
            if (allocated(cst_node%text)) then
                text_string = string_t(cst_node%text)
                select type (op_node => ast_node_obj)
                type is (binary_op_node)
                    op_node%operator = cst_node%text
                end select
            end if
            
        case default
            ! Create generic AST node for unsupported CST types
            allocate(program_node :: ast_node_obj)  ! Fallback
            conv_result%result = create_error_result( &
                "Unsupported CST node type in conversion", &
                ERROR_INTERNAL, &
                component="cst_to_ast_converter", &
                context="create_ast_node_from_cst", &
                suggestion="Add support for this CST node type" &
            )
        end select
    end subroutine create_ast_node_from_cst
    
    ! Convert single CST node (public interface)
    function convert_single_cst_node(this, cst_node) result(conv_result)
        class(cst_to_ast_converter_t), intent(inout) :: this
        type(cst_node_t), intent(in) :: cst_node
        type(conversion_result_t) :: conv_result
        
        class(ast_node), allocatable :: ast_node_obj
        
        conv_result%result = success_result()
        conv_result%nodes_converted = 0
        conv_result%trivia_stripped = 0
        
        ! Create AST node from CST node
        call create_ast_node_from_cst(cst_node, ast_node_obj, conv_result)
        if (conv_result%result%is_failure()) return
        
        ! Count trivia stripped
        if (allocated(cst_node%leading_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%leading_trivia)
        end if
        if (allocated(cst_node%trailing_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%trailing_trivia)
        end if
        
        ! Set UID for bidirectional linking
        ast_node_obj%uid%value = cst_node%uid
        
        ! Add to arena
        call this%ast_arena%push(ast_node_obj)
        conv_result%ast_root_index = this%ast_arena%size
        conv_result%nodes_converted = 1
    end function convert_single_cst_node
    
    ! Infer literal type from text content
    subroutine infer_literal_type(text, lit_node)
        character(len=*), intent(in) :: text
        type(literal_node), intent(inout) :: lit_node
        
        character(len=len(text)) :: trimmed_text
        integer :: dot_pos, int_val, ios
        real :: real_val
        logical :: logical_val
        
        trimmed_text = trim(adjustl(text))
        
        ! Check for logical literal
        if (trimmed_text == '.true.' .or. trimmed_text == '.false.') then
            lit_node%literal_type = "logical"
            logical_val = (trimmed_text == '.true.')
            lit_node%constant_logical = logical_val
            lit_node%is_constant = .true.
            lit_node%constant_type = LITERAL_LOGICAL
            return
        end if
        
        ! Check for string literal
        if ((trimmed_text(1:1) == '"' .and. trimmed_text(len_trim(trimmed_text):len_trim(trimmed_text)) == '"') .or. &
            (trimmed_text(1:1) == "'" .and. trimmed_text(len_trim(trimmed_text):len_trim(trimmed_text)) == "'")) then
            lit_node%literal_type = "character"
            lit_node%constant_type = LITERAL_STRING
            return
        end if
        
        ! Check for real literal (contains decimal point)
        dot_pos = index(trimmed_text, '.')
        if (dot_pos > 0) then
            read(trimmed_text, *, iostat=ios) real_val
            if (ios == 0) then
                lit_node%literal_type = "real"
                lit_node%constant_real = real_val
                lit_node%is_constant = .true.
                lit_node%constant_type = LITERAL_REAL
                return
            end if
        end if
        
        ! Try integer literal
        read(trimmed_text, *, iostat=ios) int_val
        if (ios == 0) then
            lit_node%literal_type = "integer"
            lit_node%constant_integer = int_val
            lit_node%is_constant = .true.
            lit_node%constant_type = LITERAL_INTEGER
            return
        end if
        
        ! Default to string if nothing else matches
        lit_node%literal_type = "character"
        lit_node%constant_type = LITERAL_STRING
    end subroutine infer_literal_type
    
    ! Get converter statistics
    function converter_get_stats(this) result(stats)
        class(cst_to_ast_converter_t), intent(in) :: this
        type(converter_stats_t) :: stats
        
        stats%total_conversions = this%conversion_count
        stats%nodes_converted = 0  ! Would need to track across conversions
        stats%trivia_entries_stripped = 0  ! Would need to track across conversions
        stats%average_conversion_time_ms = 0  ! Would need timing infrastructure
    end function converter_get_stats
    
    ! Reset converter state
    subroutine converter_reset(this)
        class(cst_to_ast_converter_t), intent(inout) :: this
        
        call this%ast_arena%clear()
        this%conversion_count = 0
    end subroutine converter_reset
    
    ! Set converter options
    subroutine converter_set_options(this, strip_trivia, preserve_positions, validate_uids)
        class(cst_to_ast_converter_t), intent(inout) :: this
        logical, intent(in), optional :: strip_trivia
        logical, intent(in), optional :: preserve_positions
        logical, intent(in), optional :: validate_uids
        
        if (present(strip_trivia)) this%strip_trivia = strip_trivia
        if (present(preserve_positions)) this%preserve_positions = preserve_positions
        if (present(validate_uids)) this%validate_uids = validate_uids
    end subroutine converter_set_options
    
end module cst_to_ast_converter