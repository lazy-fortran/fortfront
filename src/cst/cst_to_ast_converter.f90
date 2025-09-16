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
        
        ! Convert root node and all children using an explicit work stack
        call convert_node_recursive(this, cst_arena, root_cst, 0, &
                                   ast_index, conv_result)
        
        if (conv_result%result%is_failure()) return
        
        conv_result%ast_root_index = ast_index
        this%conversion_count = this%conversion_count + 1
    end function convert_cst_tree_to_ast
    
    ! Count trivia elements that will be stripped from CST node
    subroutine count_trivia_stripped(cst_node, conv_result)
        type(cst_node_t), intent(in) :: cst_node
        type(conversion_result_t), intent(inout) :: conv_result
        
        if (allocated(cst_node%leading_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%leading_trivia)
        end if
        if (allocated(cst_node%trailing_trivia)) then
            conv_result%trivia_stripped = conv_result%trivia_stripped + &
                                         size(cst_node%trailing_trivia)
        end if
    end subroutine count_trivia_stripped
    
    ! Set source position information on AST node if preservation enabled
    subroutine preserve_node_position(this, cst_node, ast_node_obj)
        class(cst_to_ast_converter_t), intent(in) :: this
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), intent(inout) :: ast_node_obj
        
        if (this%preserve_positions) then
            ! Map CST positions to AST line/column (simplified mapping)
            ast_node_obj%line = max(1, cst_node%start_pos)
            ast_node_obj%column = 1  ! Simplified - would need lexer context for precise column
        end if
    end subroutine preserve_node_position
    
    ! Convert single CST node to AST node (iterative helper)
    subroutine convert_node_recursive(this, cst_arena, cst_node, &
                                     parent_index, ast_index, conv_result)
        class(cst_to_ast_converter_t), intent(inout) :: this
        type(cst_arena_t), intent(in) :: cst_arena
        type(cst_node_t), intent(in) :: cst_node
        integer, intent(in) :: parent_index
        integer, intent(out) :: ast_index
        type(conversion_result_t), intent(inout) :: conv_result
        
        type(cst_handle_t), allocatable :: handle_stack(:)
        integer, allocatable :: parent_stack(:)
        integer :: stack_top, stack_cap
        integer :: current_parent, current_ast
        type(cst_handle_t) :: current_handle, child_handle
        type(cst_node_t) :: current_node
        integer :: root_ast
        logical :: failed

        stack_cap = 64
        allocate(handle_stack(stack_cap))
        allocate(parent_stack(stack_cap))
        stack_top = 0
        failed = .false.

        call convert_single_node(cst_node, parent_index, root_ast, failed)
        if (failed) then
            ast_index = 0
            return
        end if

        call push_child_nodes(cst_node, root_ast)

        do while (stack_top > 0 .and. .not. failed)
            call pop_stack(current_handle, current_parent)
            current_node = cst_arena%get(current_handle)
            if (current_node%kind < 0) cycle
            call convert_single_node(current_node, current_parent, current_ast, failed)
            if (.not. failed) call push_child_nodes(current_node, current_ast)
        end do

        ast_index = root_ast

    contains

        subroutine ensure_capacity(required)
            integer, intent(in) :: required
            type(cst_handle_t), allocatable :: tmp_handles(:)
            integer, allocatable :: tmp_parents(:)
            integer :: new_cap
            if (required <= stack_cap) return
            new_cap = max(stack_cap*2, required)
            allocate(tmp_handles(new_cap), tmp_parents(new_cap))
            if (stack_cap > 0) then
                tmp_handles(1:stack_top) = handle_stack(1:stack_top)
                tmp_parents(1:stack_top) = parent_stack(1:stack_top)
            end if
            call move_alloc(tmp_handles, handle_stack)
            call move_alloc(tmp_parents, parent_stack)
            stack_cap = new_cap
        end subroutine ensure_capacity

        subroutine push_stack(handle, parent)
            type(cst_handle_t), intent(in) :: handle
            integer, intent(in) :: parent
            call ensure_capacity(stack_top + 1)
            stack_top = stack_top + 1
            handle_stack(stack_top) = handle
            parent_stack(stack_top) = parent
        end subroutine push_stack

        subroutine pop_stack(handle, parent)
            type(cst_handle_t), intent(out) :: handle
            integer, intent(out) :: parent
            handle = handle_stack(stack_top)
            parent = parent_stack(stack_top)
            stack_top = stack_top - 1
        end subroutine pop_stack

        subroutine push_child_nodes(node, parent_ast)
            type(cst_node_t), intent(in) :: node
            integer, intent(in) :: parent_ast
            integer :: child_idx
            if (.not. allocated(node%children)) return
            do child_idx = size(node%children), 1, -1
                child_handle%index = node%children(child_idx)
                child_handle%generation = cst_arena%global_generation
                call push_stack(child_handle, parent_ast)
            end do
        end subroutine push_child_nodes

        subroutine convert_single_node(node, parent, new_ast_index, failed_local)
            type(cst_node_t), intent(in) :: node
            integer, intent(in) :: parent
            integer, intent(out) :: new_ast_index
            logical, intent(out) :: failed_local
            class(ast_node), allocatable :: ast_node_obj

            failed_local = .false.
            new_ast_index = 0

            call create_ast_node_from_cst(node, ast_node_obj, conv_result)
            if (conv_result%result%is_failure()) then
                failed_local = .true.
                return
            end if

            call count_trivia_stripped(node, conv_result)
            ast_node_obj%uid%value = node%uid
            call preserve_node_position(this, node, ast_node_obj)

            if (parent > 0) then
                call this%ast_arena%push(ast_node_obj, parent_index=parent)
            else
                call this%ast_arena%push(ast_node_obj)
            end if

            new_ast_index = this%ast_arena%size
            conv_result%nodes_converted = conv_result%nodes_converted + 1
        end subroutine convert_single_node

    end subroutine convert_node_recursive
    
    ! Create identifier AST node from CST node text
    subroutine create_identifier_ast_node(cst_node, ast_node_obj)
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), allocatable, intent(out) :: ast_node_obj
        
        type(string_t) :: text_string
        
        allocate(identifier_node :: ast_node_obj)
        if (allocated(cst_node%text)) then
            text_string = string_t(cst_node%text)
            select type (id_node => ast_node_obj)
            type is (identifier_node)
                id_node%name = cst_node%text
            end select
        end if
    end subroutine create_identifier_ast_node
    
    ! Create literal AST node from CST node text with type inference
    subroutine create_literal_ast_node(cst_node, ast_node_obj)
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), allocatable, intent(out) :: ast_node_obj
        
        type(string_t) :: text_string
        
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
    end subroutine create_literal_ast_node
    
    ! Create operator AST node from CST node text
    subroutine create_operator_ast_node(cst_node, ast_node_obj)
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), allocatable, intent(out) :: ast_node_obj
        
        type(string_t) :: text_string
        
        allocate(binary_op_node :: ast_node_obj)
        if (allocated(cst_node%text)) then
            text_string = string_t(cst_node%text)
            select type (op_node => ast_node_obj)
            type is (binary_op_node)
                op_node%operator = cst_node%text
            end select
        end if
    end subroutine create_operator_ast_node
    
    ! Create appropriate AST node from CST node
    subroutine create_ast_node_from_cst(cst_node, ast_node_obj, conv_result)
        type(cst_node_t), intent(in) :: cst_node
        class(ast_node), allocatable, intent(out) :: ast_node_obj
        type(conversion_result_t), intent(inout) :: conv_result
        
        select case (cst_node%kind)
        case (CST_PROGRAM)
            allocate(program_node :: ast_node_obj)
        case (CST_ASSIGNMENT)
            allocate(assignment_node :: ast_node_obj)
        case (CST_CALL)
            allocate(subroutine_call_node :: ast_node_obj)
        case (CST_IDENTIFIER)
            call create_identifier_ast_node(cst_node, ast_node_obj)
        case (CST_LITERAL)
            call create_literal_ast_node(cst_node, ast_node_obj)
        case (CST_OPERATOR)
            call create_operator_ast_node(cst_node, ast_node_obj)
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
    
    ! Check if text represents a logical literal (.true./.false.)
    function check_logical_literal(trimmed_text, lit_node) result(is_logical)
        character(len=*), intent(in) :: trimmed_text
        type(literal_node), intent(inout) :: lit_node
        logical :: is_logical
        
        logical :: logical_val
        
        is_logical = .false.
        if (trimmed_text == '.true.' .or. trimmed_text == '.false.') then
            lit_node%literal_type = "logical"
            logical_val = (trimmed_text == '.true.')
            lit_node%constant_logical = logical_val
            lit_node%is_constant = .true.
            lit_node%constant_type = LITERAL_LOGICAL
            lit_node%literal_kind = LITERAL_LOGICAL
            is_logical = .true.
        end if
    end function check_logical_literal
    
    ! Check if text represents a string literal (quoted)
    function check_string_literal(trimmed_text, lit_node) result(is_string)
        character(len=*), intent(in) :: trimmed_text
        type(literal_node), intent(inout) :: lit_node
        logical :: is_string
        
        is_string = .false.
        if ((trimmed_text(1:1) == '"' .and. trimmed_text(len_trim(trimmed_text):len_trim(trimmed_text)) == '"') .or. &
            (trimmed_text(1:1) == "'" .and. trimmed_text(len_trim(trimmed_text):len_trim(trimmed_text)) == "'")) then
            lit_node%literal_type = "character"
            lit_node%constant_type = LITERAL_STRING
            lit_node%literal_kind = LITERAL_STRING
            is_string = .true.
        end if
    end function check_string_literal
    
    ! Check if text represents a real literal (contains decimal point)
    function check_real_literal(trimmed_text, lit_node) result(is_real)
        character(len=*), intent(in) :: trimmed_text
        type(literal_node), intent(inout) :: lit_node
        logical :: is_real
        
        integer :: dot_pos, ios
        real :: real_val
        
        is_real = .false.
        dot_pos = index(trimmed_text, '.')
        if (dot_pos > 0) then
            read(trimmed_text, *, iostat=ios) real_val
            if (ios == 0) then
                lit_node%literal_type = "real"
                lit_node%constant_real = real_val
                lit_node%is_constant = .true.
                lit_node%constant_type = LITERAL_REAL
                lit_node%literal_kind = LITERAL_REAL
                is_real = .true.
            end if
        end if
    end function check_real_literal
    
    ! Check if text represents an integer literal
    function check_integer_literal(trimmed_text, lit_node) result(is_integer)
        character(len=*), intent(in) :: trimmed_text
        type(literal_node), intent(inout) :: lit_node
        logical :: is_integer
        
        integer :: int_val, ios
        
        is_integer = .false.
        read(trimmed_text, *, iostat=ios) int_val
        if (ios == 0) then
            lit_node%literal_type = "integer"
            lit_node%constant_integer = int_val
            lit_node%is_constant = .true.
            lit_node%constant_type = LITERAL_INTEGER
            lit_node%literal_kind = LITERAL_INTEGER
            is_integer = .true.
        end if
    end function check_integer_literal
    
    ! Infer literal type from text content
    subroutine infer_literal_type(text, lit_node)
        character(len=*), intent(in) :: text
        type(literal_node), intent(inout) :: lit_node
        
        character(len=len(text)) :: trimmed_text
        
        trimmed_text = trim(adjustl(text))
        
        ! Check each literal type in order of specificity
        if (check_logical_literal(trimmed_text, lit_node)) return
        if (check_string_literal(trimmed_text, lit_node)) return
        if (check_real_literal(trimmed_text, lit_node)) return
        if (check_integer_literal(trimmed_text, lit_node)) return
        
        ! Default to string if nothing else matches
        lit_node%literal_type = "character"
        lit_node%constant_type = LITERAL_STRING
        lit_node%literal_kind = LITERAL_STRING
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
