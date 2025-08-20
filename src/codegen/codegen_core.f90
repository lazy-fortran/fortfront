module codegen_core
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_core, only: component_access_node, range_subscript_node
    use ast_nodes_control, only: associate_node
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, derived_type_node
    use type_system_hm
    use string_types, only: string_t
    use codegen_indent
    implicit none
    private

    ! Type for storing parameter information during codegen
    ! Used for mapping function/subroutine parameters to their attributes
    type :: parameter_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent_str
        logical :: is_optional
    end type parameter_info_t

    ! Context for function indentation
    logical :: context_has_executable_before_contains = .false.
    
    ! Type standardization configuration
    logical, save :: standardize_types_enabled = .true.

    ! Public interface for code generation
    public :: generate_code_from_arena, generate_code_polymorphic, safe_generate_code_from_arena
    public :: set_type_standardization, get_type_standardization

contains

    ! Generate code from AST arena
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (assignment_node)
            code = generate_code_assignment(arena, node, node_index)
        type is (binary_op_node)
            code = generate_code_binary_op(arena, node, node_index)
        type is (component_access_node)
            code = generate_code_component_access(arena, node, node_index)
        type is (range_subscript_node)
            code = generate_code_range_subscript(arena, node, node_index)
        type is (program_node)
            code = generate_code_program(arena, node, node_index)
        type is (call_or_subscript_node)
            code = generate_code_call_or_subscript(arena, node, node_index)
        type is (subroutine_call_node)
            code = generate_code_subroutine_call(arena, node, node_index)
        type is (function_def_node)
            code = generate_code_function_def(arena, node, node_index)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(arena, node, node_index)
        type is (print_statement_node)
            code = generate_code_print_statement(arena, node, node_index)
        type is (declaration_node)
            code = generate_code_declaration(arena, node, node_index)
        type is (parameter_declaration_node)
            code = generate_code_parameter_declaration(arena, node, node_index)
        type is (if_node)
            code = generate_code_if(arena, node, node_index)
        type is (do_loop_node)
            code = generate_code_do_loop(arena, node, node_index)
        type is (do_while_node)
            code = generate_code_do_while(arena, node, node_index)
        type is (select_case_node)
            code = generate_code_select_case(arena, node, node_index)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (contains_node)
            code = "contains"
        type is (end_statement_node)
            code = "end"
        type is (array_literal_node)
            code = generate_code_array_literal(arena, node, node_index)
        type is (stop_node)
            code = generate_code_stop(arena, node, node_index)
        type is (return_node)
            code = generate_code_return(arena, node, node_index)
        type is (goto_node)
            code = generate_code_goto(arena, node, node_index)
        type is (error_stop_node)
            code = generate_code_error_stop(arena, node, node_index)
        type is (cycle_node)
            code = generate_code_cycle(arena, node, node_index)
        type is (exit_node)
            code = generate_code_exit(arena, node, node_index)
        type is (where_node)
            code = generate_code_where(arena, node, node_index)
        type is (forall_node)
            code = generate_code_forall(arena, node, node_index)
        type is (associate_node)
            code = generate_code_associate(arena, node, node_index)
        type is (module_node)
            code = generate_code_module(arena, node, node_index)
        type is (comment_node)
            code = generate_code_comment(node)
        type is (range_expression_node)
            code = generate_code_range_expression(arena, node, node_index)
        type is (array_bounds_node)
            code = generate_code_array_bounds(arena, node, node_index)
        type is (array_slice_node)
            code = generate_code_array_slice(arena, node, node_index)
        type is (array_operation_node)
            code = generate_code_array_operation(arena, node, node_index)
        type is (implicit_statement_node)
            code = generate_code_implicit_statement(node)
        type is (derived_type_node)
            code = generate_code_derived_type(arena, node, node_index)
        class default
            code = "! Unknown node type"
        end select
    end function generate_code_from_arena

    ! Generate code for literal node
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Return the literal value with proper formatting
        select case (node%literal_kind)
        case (LITERAL_INTEGER)
            ! Integer literals: return value directly
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "0"  ! Default integer literal
            end if
        case (LITERAL_STRING)
            ! Special case for implicit none
            if (node%value == "implicit none") then
                code = "implicit none"
                ! Special case for comments (strings starting with "!")
            else if (len(node%value) > 0 .and. node%value(1:1) == "!") then
                code = node%value  ! Keep comments as-is, no quotes
                ! String literals need quotes if not already present
            else if (len_trim(node%value) == 0) then
                code = ""  ! Skip empty literals (parser placeholders)
            else if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. &
                     node%value(1:1) /= "'") then
                code = '"'//node%value//'"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, conditionally add 'd0' suffix
            ! based on standardization setting
            if (standardize_types_enabled) then
                ! Ensure double precision by adding 'd0' suffix if needed
                if (index(node%value, 'd') == 0 .and. &
                    index(node%value, 'D') == 0 .and. &
                    index(node%value, '_') == 0) then
                    code = node%value//"d0"
                else
                    code = node%value
                end if
            else
                ! When standardization is disabled, preserve original literal format
                code = node%value
            end if
        case (LITERAL_LOGICAL)
            ! Logical literals: return value directly
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = ".false."  ! Default logical literal
            end if
        case default
            ! Handle invalid/empty literals safely
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "! Invalid literal node"
            end if
        end select
    end function generate_code_literal

    ! Generate code for identifier node
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simply return the identifier name
        code = node%name
    end function generate_code_identifier

    ! Generate code for assignment node
    function generate_code_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, value_code

        ! Generate code for target
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            target_code = generate_code_from_arena(arena, node%target_index)
        else
            target_code = "???"
        end if

        ! Generate code for value
        if (node%value_index > 0 .and. node%value_index <= arena%size) then
            value_code = generate_code_from_arena(arena, node%value_index)
            
        else
            value_code = "???"
        end if

        ! Combine target and value
        code = target_code//" = "//value_code
        
        ! Apply line continuation if the assignment exceeds line length limit
        code = add_line_continuations(code)
    end function generate_code_assignment

    ! Helper function to determine operator precedence
    function get_operator_precedence(operator) result(precedence)
        character(len=*), intent(in) :: operator
        integer :: precedence
        
        select case (trim(operator))
        case ("**")
            precedence = 10  ! Highest precedence (exponentiation, right-associative)
        case ("*", "/")
            precedence = 8   ! Multiplication and division
        case ("+", "-", "//")
            precedence = 6   ! Addition, subtraction, concatenation
        case ("==", "/=", "<", "<=", ">", ">=")
            precedence = 4   ! Comparisons
        case (".and.")
            precedence = 3   ! Logical AND
        case (".or.")
            precedence = 2   ! Logical OR
        case (".eqv.", ".neqv.")
            precedence = 1   ! Logical equivalence (lowest precedence)
        case default
            precedence = 0   ! Unknown operators get lowest precedence
        end select
    end function get_operator_precedence

    ! Helper function to check if parentheses are needed for an operand
    function needs_parentheses(arena, operand_index, parent_op, is_left_operand) result(needs)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operand_index
        character(len=*), intent(in) :: parent_op
        logical, intent(in) :: is_left_operand
        logical :: needs
        integer :: operand_prec, parent_prec
        
        needs = .false.
        
        if (operand_index <= 0 .or. operand_index > arena%size) return
        if (.not. allocated(arena%entries(operand_index)%node)) return
        
        ! Check if operand is a binary operation
        select type (operand_node => arena%entries(operand_index)%node)
        type is (binary_op_node)
            operand_prec = get_operator_precedence(operand_node%operator)
            parent_prec = get_operator_precedence(parent_op)
            
            if (operand_prec < parent_prec) then
                ! Lower precedence always needs parentheses
                needs = .true.
            else if (operand_prec == parent_prec .and. .not. is_left_operand) then
                ! Equal precedence on right side needs parentheses for left-associative operators
                ! Exception: ** is right-associative, so a ** b ** c = a ** (b ** c)
                if (trim(parent_op) /= "**") then
                    needs = .true.
                end if
            end if
        end select
    end function needs_parentheses

    ! Generate code for binary operation node
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate code for operands with parentheses when needed
        if (node%left_index > 0 .and. node%left_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%left_index)
            ! Add parentheses if left operand has lower precedence
            if (needs_parentheses(arena, node%left_index, node%operator, .true.)) then
                left_code = "(" // left_code // ")"
            end if
        else
            left_code = ""
        end if

        if (node%right_index > 0 .and. node%right_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%right_index)
            ! Add parentheses if right operand has lower or equal precedence (for left-associative ops)
            if (needs_parentheses(arena, node%right_index, node%operator, .false.)) then
                right_code = "(" // right_code // ")"
            end if
        else
            right_code = ""
        end if

        ! Combine with operator - precedence-aware spacing
        if (trim(node%operator) == ':') then
            ! Array slicing operator
            if (len(left_code) == 0) then
                ! Empty lower bound: :upper
                code = ":"//right_code
            else if (len(right_code) == 0) then
                ! Empty upper bound: lower:
                code = left_code//":"
            else
                ! Both bounds: lower:upper
                code = left_code//":"//right_code
            end if
        else if (trim(node%operator) == '**') then
            ! Exponentiation gets no spaces (highest precedence)
            code = left_code//node%operator//right_code
        else if (trim(node%operator) == '*' .or. trim(node%operator) == '/') then
            ! Multiplication and division get no spaces (original convention)
            code = left_code//node%operator//right_code
        else
            ! All other operators (comparisons, logical, etc.) get spaces around them
            code = left_code//" "//node%operator//" "//right_code
        end if
    end function generate_code_binary_op

    ! Generate code for component access node
    function generate_code_component_access(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(component_access_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: base_code

        ! Generate code for base expression
        if (node%base_expr_index > 0 .and. node%base_expr_index <= arena%size) then
            base_code = generate_code_from_arena(arena, node%base_expr_index)
        else
            base_code = "<invalid_base>"
        end if

        ! Combine base expression with component name
        if (allocated(node%component_name)) then
            code = base_code // "%" // node%component_name
        else
            code = base_code // "%<missing_component>"
        end if

    end function generate_code_component_access

    ! Generate code for range subscript node (array slice or character substring)
    function generate_code_range_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: base_code, start_code, end_code
        
        ! Generate code for base expression
        if (node%base_expr_index > 0 .and. node%base_expr_index <= arena%size) then
            base_code = generate_code_from_arena(arena, node%base_expr_index)
        else
            base_code = "<invalid_base>"
        end if
        
        ! Generate start position code
        if (node%start_index > 0 .and. node%start_index <= arena%size) then
            start_code = generate_code_from_arena(arena, node%start_index)
        else
            start_code = ""  ! Empty means from beginning
        end if
        
        ! Generate end position code
        if (node%end_index > 0 .and. node%end_index <= arena%size) then
            end_code = generate_code_from_arena(arena, node%end_index)
        else
            end_code = ""  ! Empty means to end
        end if
        
        ! Build range subscript expression (same syntax for array slice and substring)
        code = base_code // "(" // start_code // ":" // end_code // ")"
        
    end function generate_code_range_subscript

    ! Generate code for program node
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: use_statements, declarations, exec_statements
        integer, allocatable :: child_indices(:)
        integer :: i
        character(len=:), allocatable :: stmt_code
    logical :: has_declarations, has_executable, has_implicit_none, has_var_declarations

        ! Reset indentation for top-level program
        call reset_indent()
        
        ! Special handling for multiple top-level units
        if (node%name == "__MULTI_UNIT__") then
            code = ""
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. &
                        node%body_indices(i) <= arena%size) then
                        stmt_code = generate_code_from_arena(arena, &
                                                             node%body_indices(i))
                        if (len_trim(stmt_code) > 0) then
                            ! Skip empty main programs that only contain implicit none
                            ! This is a regression fix for Issue #321 mixed construct handling
                            block
                                logical :: is_empty_main_program
                                is_empty_main_program = .false.
                                
                                ! Check if this is a main program with only implicit none
                                if (index(stmt_code, "program main") > 0 .and. &
                                    index(stmt_code, "implicit none") > 0 .and. &
                                    index(stmt_code, "end program main") > 0) then
                                    ! Debug: Found potential empty main program
                                    ! print *, "DEBUG: Found potential empty main program"
                                    ! Count non-whitespace, non-comment lines
                                    block
                                        character(len=:), allocatable :: lines(:)
                                        integer :: line_count, meaningful_lines, j
                                        character(len=1000) :: temp_line
                                        
                                        ! Simple check: if the code only has 3 non-empty lines
                                        ! (program main, implicit none, end program main)
                                        ! then it's likely an empty program
                                        meaningful_lines = 0
                                        line_count = 1
                                        do j = 1, len_trim(stmt_code)
                                            if (stmt_code(j:j) == new_line('A')) then
                                                line_count = line_count + 1
                                            end if
                                        end do
                                        
                                        ! Check if this looks like an empty main program
                                        ! by looking for the specific pattern
                                        if (line_count <= 4 .and. &  ! Very few lines
                                            index(stmt_code, "use ") == 0 .and. &  ! No use statements
                                            index(stmt_code, "call ") == 0 .and. &  ! No calls
                                            index(stmt_code, "=") == 0 .and. &  ! No assignments
                                            index(stmt_code, "write") == 0 .and. &  ! No write statements
                                            index(stmt_code, "print") == 0) then  ! No print statements
                                            is_empty_main_program = .true.
                                        end if
                                    end block
                                end if
                                
                                if (.not. is_empty_main_program) then
                                    if (len(code) > 0) then
                                        code = code//new_line('A')
                                    end if
                                    code = code//stmt_code
                                end if
                            end block
                        end if
                    end if
                end do
            end if
            return
        end if

        ! Initialize sections
        use_statements = ""
        declarations = ""
        exec_statements = ""
        has_declarations = .false.
        has_executable = .false.
        has_implicit_none = .false.
        has_var_declarations = .false.

        ! Use body_indices from program_node, not get_children
        if (allocated(node%body_indices)) then
            allocate (child_indices(size(node%body_indices)))
            child_indices = node%body_indices
        else
            allocate (child_indices(0))
        end if

        ! Process all statements, separating by type
        do i = 1, size(child_indices)
            if (child_indices(i) > 0 .and. child_indices(i) <= arena%size) then
                if (allocated(arena%entries(child_indices(i))%node)) then
                    select type (child_node => arena%entries(child_indices(i))%node)
                    type is (use_statement_node)
                        if (len(use_statements) > 0) then
                            use_statements = use_statements//new_line('A')
                        end if
                use_statements = use_statements//generate_code_use_statement(child_node)
                    type is (literal_node)
                        ! Check for implicit none
                        if (child_node%value == "implicit none") then
                            has_implicit_none = .true.
                            ! Only add implicit none if we haven't already processed one
                            if (len(declarations) == 0 .or. index(declarations, "implicit none") == 0) then
                                if (len(declarations) > 0) then
                                    declarations = declarations//new_line('A')
                                end if
                                declarations = declarations//"implicit none"
                                has_declarations = .true.
                            end if
                        else
                            ! Other literals go to executable section
                           stmt_code = generate_code_from_arena(arena, child_indices(i))
                            if (len(stmt_code) > 0) then
                                if (len(exec_statements) > 0) then
                                    exec_statements = exec_statements//new_line('A')
                                end if
                                exec_statements = exec_statements//stmt_code
                                has_executable = .true.
                            end if
                        end if
                    type is (declaration_node)
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(declarations) > 0) then
                                declarations = declarations//new_line('A')
                            end if
                            declarations = declarations//stmt_code
                            has_declarations = .true.
                            has_var_declarations = .true.
                        end if
                    type is (parameter_declaration_node)
                        ! Parameter declarations are handled like
                        ! regular declarations in body
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(declarations) > 0) then
                                declarations = declarations//new_line('A')
                            end if
                            declarations = declarations//stmt_code
                            has_declarations = .true.
                            has_var_declarations = .true.
                        end if
                    type is (contains_node)
                        ! Handle contains specially - it goes between exec and functions
                        ! We'll process it later, not here
                        continue
                    type is (end_statement_node)
                        ! End statement is executable - add to exec section
                        continue
                    type is (function_def_node)
                        ! Functions go after contains, not in executable section
                        continue
                    type is (subroutine_def_node)
                        ! Subroutines go after contains, not in executable section
                        continue
                    type is (identifier_node)
                        ! Skip standalone identifiers
                        ! - they are not executable statements
                        continue
                    type is (implicit_statement_node)
                        ! Handle implicit statement nodes
                        if (child_node%is_none) then
                            has_implicit_none = .true.
                            ! Only add implicit none if we haven't already processed one
                            if (len(declarations) == 0 .or. index(declarations, "implicit none") == 0) then
                                if (len(declarations) > 0) then
                                    declarations = declarations//new_line('A')
                                end if
                                declarations = declarations//"implicit none"
                                has_declarations = .true.
                            end if
                        end if
                    type is (comment_node)
                        ! Comments go in executable section
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(exec_statements) > 0) then
                                exec_statements = exec_statements//new_line('A')
                            end if
                            exec_statements = exec_statements//stmt_code
                            ! Don't set has_executable for comments alone
                        end if
                    class default
                        ! All other statements are executable
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(exec_statements) > 0) then
                                exec_statements = exec_statements//new_line('A')
                            end if
                            exec_statements = exec_statements//stmt_code
                            has_executable = .true.
                        end if
                    end select
                end if
            end if
        end do

        ! Combine everything with proper spacing
        code = "program "//node%name//new_line('A')
        call increase_indent()

        if (len(use_statements) > 0) then
            code = code//indent_lines(use_statements)//new_line('A')
        end if

        ! Add implicit none by default if not already present
        if (.not. has_implicit_none) then
            if (len(declarations) > 0) then
                declarations = "implicit none"//new_line('A')//declarations
            else
                declarations = "implicit none"
            end if
            has_declarations = .true.
        end if

        if (len(declarations) > 0) then
            code = code//indent_lines(declarations)//new_line('A')
            ! Add blank line after declarations only if we have
            ! variable declarations and executable statements
            if (has_var_declarations .and. has_executable) then
                code = code//new_line('A')
            end if
        end if

        if (len(exec_statements) > 0) then
            code = code//indent_lines(exec_statements)//new_line('A')
        end if

        ! Check for contains and functions/subroutines
        block
            logical :: has_contains, has_subprograms
            integer :: j

            has_contains = .false.
            has_subprograms = .false.

            ! Check if we have contains or subprograms
            do j = 1, size(child_indices)
                if (child_indices(j) > 0 .and. child_indices(j) <= arena%size) then
                    if (allocated(arena%entries(child_indices(j))%node)) then
                        select type (child_node => arena%entries(child_indices(j))%node)
                        type is (contains_node)
                            has_contains = .true.
                        type is (end_statement_node)
                            ! End statement doesn't affect program structure
                            continue
                        type is (function_def_node)
                            has_subprograms = .true.
                        type is (subroutine_def_node)
                            has_subprograms = .true.
                        end select
                    end if
                end if
            end do

            ! Generate contains and subprograms
            if (has_contains .or. has_subprograms) then
                code = code//"contains"//new_line('A')

                ! Generate functions and subroutines
                do j = 1, size(child_indices)
                    if (child_indices(j) > 0 .and. child_indices(j) <= arena%size) then
                        if (allocated(arena%entries(child_indices(j))%node)) then
                        select type (child_node => arena%entries(child_indices(j))%node)
                            type is (function_def_node)
                           stmt_code = generate_code_from_arena(arena, child_indices(j))
                                ! Indent function lines
                                code = code//indent_lines(stmt_code)//new_line('A')
                            type is (subroutine_def_node)
                           stmt_code = generate_code_from_arena(arena, child_indices(j))
                                ! Indent subroutine lines
                                code = code//indent_lines(stmt_code)//new_line('A')
                            end select
                        end if
                    end if
                end do
            end if
        end block

        call decrease_indent()
        code = code//"end program "//node%name
    end function generate_code_program

    ! Generate code for call_or_subscript node
    ! (handles both function calls and array indexing)
    function generate_code_call_or_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        character(len=:), allocatable :: arg_code
        integer :: i
        logical :: is_array_slice
        type(binary_op_node), pointer :: bin_op

        ! Generate arguments
        args_code = ""
        if (allocated(node%arg_indices)) then
            ! Check if this might be array slicing
            ! (single argument that's a binary op with ":")
            is_array_slice = .false.
            if (size(node%arg_indices) == 1 .and. node%arg_indices(1) > 0) then
                select type (arg_node => arena%entries(node%arg_indices(1))%node)
                type is (binary_op_node)
                    if (trim(arg_node%operator) == ":") then
                        is_array_slice = .true.
                    end if
                end select
            end if
            
            do i = 1, size(node%arg_indices)
                if (len(args_code) > 0 .and. .not. is_array_slice) then
                    args_code = args_code//", "
                end if
                if (node%arg_indices(i) > 0) then
                    arg_code = generate_code_from_arena(arena, node%arg_indices(i))
                    args_code = args_code//arg_code
                end if
            end do
        end if

        ! Combine function name and arguments
        code = node%name//"("//args_code//")"
        
        ! The disambiguation flag (node%is_array_access) is available for backends
        ! that need to distinguish between array access and function calls
    end function generate_code_call_or_subscript

    ! Generate code for subroutine call node
    function generate_code_subroutine_call(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Generate arguments
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (len(args_code) > 0) then
                    args_code = args_code//", "
                end if
                if (node%arg_indices(i) > 0) then
             args_code = args_code//generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
        end if

        ! Generate call statement
        if (len(args_code) > 0) then
            code = "call "//node%name//"("//args_code//")"
        else
            code = "call "//node%name
        end if
    end function generate_code_subroutine_call

    ! Polymorphic code generation interface
    function generate_code_polymorphic(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = generate_code_from_arena(arena, node_index)
    end function generate_code_polymorphic

    ! Generate code for function definition
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: return_type_code, params_code, body_code
        integer :: i

        ! Start function definition with return type
        if (allocated(node%return_type) .and. len_trim(node%return_type) > 0) then
            code = node%return_type//" function "//node%name
        else
            code = "function "//node%name
        end if

        ! Generate parameters
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code//"("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code//", "
           if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    params_code = generate_code_from_arena(arena, node%param_indices(i))
                    code = code//params_code
                end if
            end do
            code = code//")"
        else
            code = code//"()"
        end if

        ! Add result clause if present
        if (allocated(node%result_variable) .and. len_trim(node%result_variable) > 0) then
            code = code//" result("//node%result_variable//")"
        end if

        code = code//new_line('a')

        ! Build parameter map by matching parameter names to body declarations
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, map_idx, j
            character(len=:), allocatable :: param_name
            
            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)
            
            allocate(param_map(param_count))
            
            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.
                
                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code//generate_grouped_body_with_params(arena, &
                                    node%body_indices, "    ", param_map, node)
            end if
        end block

        ! End function
        code = code//"end function "//node%name
    end function generate_code_function_def

    ! Generate code for subroutine definition
    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: params_code, body_code
        integer :: i

        ! Start subroutine definition
        code = "subroutine "//node%name

        ! Generate parameters
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code//"("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code//", "
           if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    params_code = generate_code_from_arena(arena, node%param_indices(i))
                    code = code//params_code
                end if
            end do
            code = code//")"
        else
            code = code//"()"
        end if
        code = code//new_line('a')
        
        ! Build parameter map by matching parameter names to body declarations  
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, map_idx, j
            character(len=:), allocatable :: param_name
            
            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)
            
            allocate(param_map(param_count))
            
            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.
                
                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code//generate_grouped_body_with_params(arena, &
                                    node%body_indices, "    ", param_map, node)
            end if
        end block

        ! End subroutine
        code = code//"end subroutine "//node%name
    end function generate_code_subroutine_def

    ! Generate code for print statement
    function generate_code_print_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: arg_code
        integer :: i

        ! Start print statement
        code = "print "//node%format_spec

        ! Generate arguments
        if (allocated(node%expression_indices) .and. &
            size(node%expression_indices) > 0) then
            do i = 1, size(node%expression_indices)
                code = code//", "
               if (node%expression_indices(i) > 0 .and. &
                   node%expression_indices(i) <= arena%size) then
                    arg_code = generate_code_from_arena(arena, &
                                                       node%expression_indices(i))
                    code = code//arg_code
                end if
            end do
        end if
    end function generate_code_print_statement
    
    ! Generate code for STOP statement
    function generate_code_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stop_code_str
        
        code = with_indent("stop")
        
        ! Add stop code or message if present
        if (allocated(node%stop_message) .and. len_trim(node%stop_message) > 0) then
            code = code//" "//node%stop_message
        else if (node%stop_code_index > 0 .and. node%stop_code_index <= arena%size) then
            if (allocated(arena%entries(node%stop_code_index)%node)) then
                stop_code_str = generate_code_from_arena(arena, node%stop_code_index)
                code = code//" "//stop_code_str
            else
                code = code//" 0"  ! Default stop code
            end if
        end if
    end function generate_code_stop
    
    ! Generate code for RETURN statement
    function generate_code_return(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(return_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        code = with_indent("return")
    end function generate_code_return
    
    ! Generate code for GOTO statement
    function generate_code_goto(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(goto_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        if (allocated(node%label) .and. len_trim(node%label) > 0) then
            if (trim(node%label) == "INVALID_LABEL") then
                ! Generate a comment for invalid GOTO statements
                code = with_indent("! Invalid GOTO statement - missing label")
            else
                code = with_indent("go to "//trim(node%label))
            end if
        else
            ! Fallback for uninitialized labels
            code = with_indent("! Invalid GOTO statement - no label")
        end if
    end function generate_code_goto
    
    ! Generate code for ERROR STOP statement
    function generate_code_error_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(error_stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: error_code_str
        
        code = with_indent("error stop")
        
        ! Add error code or message if present
        if (allocated(node%error_message) .and. len_trim(node%error_message) > 0) then
            code = code//" "//node%error_message
        else if (node%error_code_index > 0 .and. node%error_code_index <= arena%size) then
            if (allocated(arena%entries(node%error_code_index)%node)) then
                error_code_str = generate_code_from_arena(arena, node%error_code_index)
                code = code//" "//error_code_str
            else
                code = code//" 1"  ! Default error code
            end if
        end if
    end function generate_code_error_stop
    
    ! Generate code for CYCLE statement
    function generate_code_cycle(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(cycle_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        if (allocated(node%label) .and. len_trim(node%label) > 0) then
            code = with_indent("cycle "//node%label)
        else
            code = with_indent("cycle")
        end if
    end function generate_code_cycle
    
    ! Generate code for EXIT statement
    function generate_code_exit(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(exit_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        if (allocated(node%label) .and. len_trim(node%label) > 0) then
            code = with_indent("exit "//node%label)
        else
            code = with_indent("exit")
        end if
    end function generate_code_exit
    
    ! Generate code for WHERE construct
    function generate_code_where(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: mask_code, stmt_code
        integer :: i, j
        
        ! Generate mask expression
        if (node%mask_expr_index > 0 .and. node%mask_expr_index <= arena%size) then
            mask_code = generate_code_from_arena(arena, node%mask_expr_index)
        else
            mask_code = "???"
        end if
        
        ! Check if this is a single-line WHERE
        if (allocated(node%where_body_indices) .and. &
            size(node%where_body_indices) == 1 .and. &
            .not. allocated(node%elsewhere_clauses)) then
            ! Single-line WHERE
            if (node%where_body_indices(1) > 0 .and. &
                node%where_body_indices(1) <= arena%size) then
                stmt_code = generate_code_from_arena(arena, node%where_body_indices(1))
                ! Remove indentation from statement for single-line
                stmt_code = adjustl(stmt_code)
            else
                stmt_code = "???"
            end if
            code = with_indent("where ("//mask_code//") "//stmt_code)
        else
            ! Multi-line WHERE construct
            code = with_indent("where ("//mask_code//")")
            
            ! Increase indentation for body
            call increase_indent()
            
            ! Generate WHERE body
            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    if (node%where_body_indices(i) > 0 .and. &
                        node%where_body_indices(i) <= arena%size) then
                        stmt_code = generate_code_from_arena(arena, &
                                                             node%where_body_indices(i))
                        code = code//new_line('A')//stmt_code
                    end if
                end do
            end if
            
            ! Decrease indentation
            call decrease_indent()
            
            ! Generate ELSEWHERE blocks
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    if (node%elsewhere_clauses(i)%mask_index > 0) then
                        ! ELSEWHERE with mask
                        mask_code = generate_code_from_arena(arena, &
                                          node%elsewhere_clauses(i)%mask_index)
                        code = code//new_line('A')// &
                               with_indent("elsewhere ("//mask_code//")")
                    else
                        ! Final ELSEWHERE without mask
                        code = code//new_line('A')//with_indent("elsewhere")
                    end if
                    
                    ! Generate body for this elsewhere clause
                    if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                        call increase_indent()
                        
                        do j = 1, size(node%elsewhere_clauses(i)%body_indices)
                            if (node%elsewhere_clauses(i)%body_indices(j) > 0 .and. &
                                node%elsewhere_clauses(i)%body_indices(j) &
                                <= arena%size) then
                                stmt_code = generate_code_from_arena(arena, &
                                  node%elsewhere_clauses(i)%body_indices(j))
                                code = code//new_line('A')//stmt_code
                            end if
                        end do
                        
                        call decrease_indent()
                    end if
                end do
            end if
            
            code = code//new_line('A')//with_indent("end where")
        end if
    end function generate_code_where

    ! Generate code for FORALL construct
    function generate_code_forall(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(forall_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: index_spec, mask_code, stmt_code
        integer :: idx, body_idx
        
        ! Build index specifications
        index_spec = ""
        do idx = 1, node%num_indices
            if (idx > 1) index_spec = index_spec//", "
            
            ! Add index name
            if (allocated(node%index_names)) then
                index_spec = index_spec//trim(node%index_names(idx))//"="
            else
                index_spec = index_spec//"i"//trim(adjustl(int_to_string(idx)))//"="
            end if
            
            ! Add lower bound
            if (allocated(node%lower_bound_indices)) then
                if (node%lower_bound_indices(idx) > 0 .and. &
                    node%lower_bound_indices(idx) <= arena%size) then
                    index_spec = index_spec// &
                        generate_code_from_arena(arena, node%lower_bound_indices(idx))
                else
                    index_spec = index_spec//"1"
                end if
            else
                index_spec = index_spec//"1"
            end if
            
            index_spec = index_spec//":"
            
            ! Add upper bound
            if (allocated(node%upper_bound_indices)) then
                if (node%upper_bound_indices(idx) > 0 .and. &
                    node%upper_bound_indices(idx) <= arena%size) then
                    index_spec = index_spec// &
                        generate_code_from_arena(arena, node%upper_bound_indices(idx))
                else
                    index_spec = index_spec//"n"
                end if
            else
                index_spec = index_spec//"n"
            end if
            
            ! Add optional stride
            if (allocated(node%stride_indices)) then
                if (node%stride_indices(idx) > 0 .and. &
                    node%stride_indices(idx) <= arena%size) then
                    index_spec = index_spec//":"// &
                        generate_code_from_arena(arena, node%stride_indices(idx))
                end if
            end if
        end do
        
        ! Add optional mask
        if (node%has_mask .and. node%mask_expr_index > 0 .and. &
            node%mask_expr_index <= arena%size) then
            mask_code = generate_code_from_arena(arena, node%mask_expr_index)
            index_spec = index_spec//", "//mask_code
        end if
        
        ! Generate FORALL header
        code = with_indent("forall ("//index_spec//")")
        
        ! Generate body
        if (allocated(node%body_indices)) then
            call increase_indent()
            
            do body_idx = 1, size(node%body_indices)
                if (node%body_indices(body_idx) > 0 .and. &
                    node%body_indices(body_idx) <= arena%size) then
                    stmt_code = generate_code_from_arena(arena, node%body_indices(body_idx))
                    code = code//new_line('A')//stmt_code
                end if
            end do
            
            call decrease_indent()
        end if
        
        code = code//new_line('A')//with_indent("end forall")
    end function generate_code_forall

    ! Generate code for ASSOCIATE construct
    function generate_code_associate(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(associate_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: assoc_list, body_code
        integer :: i

        ! Build association list
        assoc_list = ""
        if (allocated(node%associations)) then
            do i = 1, size(node%associations)
                if (i > 1) assoc_list = assoc_list//", "
                assoc_list = assoc_list//trim(node%associations(i)%name)//" => "
                
                ! Generate expression code
                if (node%associations(i)%expr_index > 0) then
                    assoc_list = assoc_list//generate_code_from_arena(arena, &
                                                node%associations(i)%expr_index)
                end if
            end do
        end if

        ! Start ASSOCIATE construct
        code = "associate ("//assoc_list//")"

        ! Generate body code
        call increase_indent()
        body_code = ""
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (i > 1) body_code = body_code//new_line('a')
                body_code = body_code//with_indent( &
                    generate_code_from_arena(arena, node%body_indices(i)))
            end do
        end if
        call decrease_indent()

        ! Complete construct
        if (len_trim(body_code) > 0) then
            code = code//new_line('a')//body_code//new_line('a')// &
                   with_indent("end associate")
        else
            code = code//new_line('a')//with_indent("end associate")
        end if
    end function generate_code_associate

    ! Generate code for declaration
    function generate_code_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: init_code
        character(len=:), allocatable :: type_str
        integer :: i, j

        ! Determine the type string
        if (len_trim(node%type_name) > 0) then
            type_str = node%type_name
        else if (allocated(node%inferred_type)) then
            ! Handle type inference
            select case (node%inferred_type%kind)
            case (TINT)
                type_str = "integer"
            case (TREAL)
                if (standardize_types_enabled) then
                    type_str = "real(8)"
                else
                    type_str = "real"
                end if
            case (TCHAR)
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    type_str = "character(len=:)"
                else if (node%inferred_type%size > 0) then
                    type_str = "character(len="// &
                        trim(adjustl(int_to_string(node%inferred_type%size)))//")"
                else
                    type_str = "character"
                end if
            case (TLOGICAL)
                type_str = "logical"
            case default
                type_str = "real"  ! Default to real
            end select
        else
            type_str = "real"  ! Default fallback
        end if

        ! Generate basic declaration
        code = type_str

        ! Add kind if present (but not for character which uses len)
        ! Also avoid adding kind if type_str already contains it (e.g., "real(8)")
        if (node%has_kind .and. node%type_name /= "character" .and. &
            index(type_str, "(") == 0) then
            code = code//"("//trim(adjustl(int_to_string(node%kind_value)))//")"
        else if (node%type_name == "character" .and. node%has_kind) then
            ! For character, kind_value is actually the length
            code = "character(len="//trim(adjustl(int_to_string(node%kind_value)))//")"
        end if

        ! Add intent if present
        if (node%has_intent .and. allocated(node%intent)) then
            code = code//", intent("//node%intent//")"
        end if

        ! Add allocatable if present or if string needs allocatable
        if (node%is_allocatable) then
            code = code//", allocatable"
        else if (allocated(node%inferred_type)) then
            if (node%inferred_type%alloc_info%needs_allocatable_string) then
                code = code//", allocatable"
            end if
        end if
        
        ! Add optional if present
        if (node%is_optional) then
            code = code//", optional"
        end if
        
        ! Add pointer if present
        if (node%is_pointer) then
            code = code//", pointer"
        end if
        
        ! Add target if present
        if (node%is_target) then
            code = code//", target"
        end if
        
        ! Add parameter if present
        if (node%is_parameter) then
            code = code//", parameter"
        end if

        ! Add variable names - handle both single and multi declarations
        code = code//" :: "
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            ! Multi-variable declaration
            do i = 1, size(node%var_names)
                if (i > 1) code = code//", "
                code = code//trim(node%var_names(i))
                ! Add dimensions per variable if needed
                if (node%is_array .and. allocated(node%dimension_indices)) then
                    code = code//"("
                    do j = 1, size(node%dimension_indices)
                        if (j > 1) code = code//","
                        if (node%dimension_indices(j) > 0 .and. &
                            node%dimension_indices(j) <= arena%size) then
                            code = code//generate_code_from_arena(arena, &
                                                                 node%dimension_indices(j))
                        else
                            code = code//":"  ! Default for unspecified dimensions
                        end if
                    end do
                    code = code//")"
                end if
            end do
        else
            ! Single variable declaration
            code = code//node%var_name
            
            ! Add array dimensions if present
            if (node%is_array .and. allocated(node%dimension_indices)) then
                ! Generate dimension expressions
                code = code//"("
                do i = 1, size(node%dimension_indices)
                    if (i > 1) code = code//","
                    if (node%dimension_indices(i) > 0 .and. &
                        node%dimension_indices(i) <= arena%size) then
                        code = code//generate_code_from_arena(arena, node%dimension_indices(i))
                    else
                        code = code//":"  ! Default for unspecified dimensions
                    end if
                end do
                code = code//")"
            end if
        end if

        ! Add initializer if present
        if (node%initializer_index > 0 .and. node%initializer_index <= arena%size) then
            init_code = generate_code_from_arena(arena, node%initializer_index)
            code = code//" = "//init_code
        end if
    end function generate_code_declaration

    ! Generate code for parameter declaration
    function generate_code_parameter_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! In parameter lists, only output the parameter name
        ! The type information is used in the body declarations
        code = node%name

    end function generate_code_parameter_declaration

    ! Generate code for if statement
    function generate_code_if(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: i

        ! Generate if condition
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ".true."
        end if

        code = with_indent("if ("//cond_code//") then")//new_line('a')
        call increase_indent()

        ! Generate then body
        if (allocated(node%then_body_indices)) then
            do i = 1, size(node%then_body_indices)
   if (node%then_body_indices(i) > 0 .and. node%then_body_indices(i) <= arena%size) then
                  body_code = generate_code_from_arena(arena, node%then_body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        ! Generate elseif blocks
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                ! Generate elseif condition
                if (node%elseif_blocks(i)%condition_index > 0 .and. &
                    node%elseif_blocks(i)%condition_index <= arena%size) then
      cond_code = generate_code_from_arena(arena, node%elseif_blocks(i)%condition_index)
                else
                    cond_code = ".true."
                end if

                call decrease_indent()
               code = code//with_indent("else if ("//cond_code//") then")//new_line('a')
                call increase_indent()

                ! Generate elseif body
                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            if (node%elseif_blocks(i)%body_indices(j) > 0 .and. &
                               node%elseif_blocks(i)%body_indices(j) <= arena%size) then
      body_code = generate_code_from_arena(arena, node%elseif_blocks(i)%body_indices(j))
                                code = code//with_indent(body_code)//new_line('a')
                            end if
                        end do
                    end block
                end if
            end do
        end if

        ! Generate else block
        if (allocated(node%else_body_indices)) then
            call decrease_indent()
            code = code//with_indent("else")//new_line('a')
            call increase_indent()
            do i = 1, size(node%else_body_indices)
   if (node%else_body_indices(i) > 0 .and. node%else_body_indices(i) <= arena%size) then
                  body_code = generate_code_from_arena(arena, node%else_body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//with_indent("end if")
    end function generate_code_if

    ! Generate code for do loop
    function generate_code_do_loop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, step_code, body_code
        integer :: i

        ! Generate loop variable and bounds
        code = with_indent("do "//node%var_name//" = ")

        if (node%start_expr_index > 0 .and. node%start_expr_index <= arena%size) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
            code = code//start_code
        else
            code = code//"1"
        end if

        code = code//", "

        if (node%end_expr_index > 0 .and. node%end_expr_index <= arena%size) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
            code = code//end_code
        else
            code = code//"1"
        end if

        if (node%step_expr_index > 0 .and. node%step_expr_index <= arena%size) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
            code = code//", "//step_code
        end if

        code = code//new_line('a')
        call increase_indent()

        ! Generate body
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//"end do"
    end function generate_code_do_loop

    ! Generate code for do while loop
    function generate_code_do_while(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_while_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: i

        ! Generate condition
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ".true."
        end if

        code = with_indent("do while ("//cond_code//")")//new_line('a')
        call increase_indent()

        ! Generate body
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//"end do"
    end function generate_code_do_while

    ! Generate code for select case
    function generate_code_select_case(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, case_code, body_code
        integer :: i, j
        logical :: found_default

        ! Generate select expression
        if (node%selector_index > 0 .and. node%selector_index <= arena%size) then
            expr_code = generate_code_from_arena(arena, node%selector_index)
        else
            expr_code = "???"
        end if

        code = with_indent("select case ("//expr_code//")")//new_line('a')
        call increase_indent()

        ! Generate case blocks
        if (allocated(node%case_indices)) then
            do i = 1, size(node%case_indices)
                if (node%case_indices(i) > 0 .and. &
                    node%case_indices(i) <= arena%size) then
                    select type (case_node => arena%entries(node%case_indices(i))%node)
                    type is (case_block_node)
                        ! Generate case values
                        if (allocated(case_node%value_indices) .and. &
                            size(case_node%value_indices) > 0) then
                            case_code = "case ("
                            do j = 1, size(case_node%value_indices)
                                if (j > 1) case_code = case_code//", "
                                if (case_node%value_indices(j) > 0) then
                                    case_code = case_code// &
                                        generate_code_from_arena(arena, &
                                            case_node%value_indices(j))
                                end if
                            end do
                            case_code = case_code//")"
                            code = code//with_indent(case_code)//new_line('a')
                        else
                            ! This might be a default case
                            ! - check if it's at the default_index
                            if (node%default_index > 0 .and. &
                                node%case_indices(i) == node%default_index) then
                                code = code//with_indent("case default")//new_line('a')
                            else
                                code = code//with_indent("case (???)")//new_line('a')
                            end if
                        end if
                        
                        call increase_indent()
                        ! Generate case body
                        if (allocated(case_node%body_indices)) then
                            do j = 1, size(case_node%body_indices)
                                if (case_node%body_indices(j) > 0) then
                                    body_code = generate_code_from_arena(arena, &
                                                       case_node%body_indices(j))
                                    code = code//with_indent(body_code)//new_line('a')
                                end if
                            end do
                        end if
                        call decrease_indent()
                    end select
                end if
            end do
        end if
        
        ! Handle default case if it exists and wasn't already handled
        if (node%default_index > 0) then
            ! Check if we haven't already processed it
            found_default = .false.
            if (allocated(node%case_indices)) then
                do i = 1, size(node%case_indices)
                    if (node%case_indices(i) == node%default_index) then
                        found_default = .true.
                        exit
                    end if
                end do
            end if
            
            if (.not. found_default) then
                select type (default_node => arena%entries(node%default_index)%node)
                type is (case_block_node)
                    code = code//with_indent("case default")//new_line('a')
                    call increase_indent()
                    if (allocated(default_node%body_indices)) then
                        do i = 1, size(default_node%body_indices)
                            if (default_node%body_indices(i) > 0) then
                                body_code = generate_code_from_arena(arena, &
                                               default_node%body_indices(i))
                                code = code//with_indent(body_code)//new_line('a')
                            end if
                        end do
                    end if
                    call decrease_indent()
                end select
            end if
        end if

        call decrease_indent()
        code = code//"end select"
    end function generate_code_select_case

    ! Generate code for use statement
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        logical :: first_item

        code = "use "//trim(node%module_name)

        if (node%has_only) then
            code = code//", only: "
            first_item = .true.

            ! Add rename list items first
            if (allocated(node%rename_list) .and. size(node%rename_list) > 0) then
                do i = 1, size(node%rename_list)
                    if (allocated(node%rename_list(i)%s)) then
                        if (len_trim(node%rename_list(i)%s) > 0) then
                            if (.not. first_item) code = code//", "
                            code = code//trim(adjustl(node%rename_list(i)%s))
                            first_item = .false.
                        end if
                    end if
                end do
            end if

            ! Add only list items
            if (allocated(node%only_list)) then
                do i = 1, size(node%only_list)
                    if (allocated(node%only_list(i)%s)) then
                        if (len_trim(node%only_list(i)%s) > 0) then
                            if (.not. first_item) code = code//", "
                            code = code//trim(adjustl(node%only_list(i)%s))
                            first_item = .false.
                        end if
                    end if
                end do
            end if
        end if
    end function generate_code_use_statement

    ! Generate code for array literal
    function generate_code_array_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: elem_code
        integer :: i
        logical :: is_char_array
        integer :: max_char_len

        ! Check if we have elements
        if (.not. allocated(node%element_indices) .or. &
            size(node%element_indices) == 0) then
            ! Empty array - use appropriate syntax based on style
            if (allocated(node%syntax_style)) then
                if (node%syntax_style == "legacy") then
                    code = "(/ /)"
                else
                    code = "[]"
                end if
            else
                code = "[]"  ! default to modern
            end if
            return
        end if

        ! Check if this is a character array with mixed lengths
        is_char_array = .false.
        max_char_len = 0
        
        if (size(node%element_indices) > 0) then
            do i = 1, size(node%element_indices)
                if (node%element_indices(i) > 0 .and. &
                    node%element_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%element_indices(i))%node)) then
                        select type (elem_node => &
                                      arena%entries(node%element_indices(i))%node)
                        type is (literal_node)
                            if (elem_node%literal_kind == LITERAL_STRING) then
                                is_char_array = .true.
                                ! Length includes quotes, so subtract 2
                                max_char_len = max(max_char_len, &
                                                 len_trim(elem_node%value) - 2)
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Generate array constructor based on syntax_style
        if (allocated(node%syntax_style) .and. node%syntax_style == "legacy") then
            code = "(/ "
        else
            code = "["
        end if
        
        ! Add character type specifier if needed
        if (is_char_array .and. max_char_len > 0) then
            code = code//"character(len="// &
                   trim(adjustl(int_to_string(max_char_len)))//") :: "
        end if
        
        ! Add each element
        do i = 1, size(node%element_indices)
            if (i > 1) code = code//", "
            if (node%element_indices(i) > 0 .and. &
                node%element_indices(i) <= arena%size) then
                ! Check if this element is a do_loop_node (implied do)
                if (allocated(arena%entries(node%element_indices(i))%node)) then
                    select type (elem_node => &
                                  arena%entries(node%element_indices(i))%node)
                    type is (do_loop_node)
                        ! Generate implied do syntax
                        elem_code = generate_implied_do(arena, elem_node, &
                                                        node%element_indices(i))
                        code = code//elem_code
                    class default
                        ! Regular element
                        elem_code = generate_code_from_arena(arena, &
                                                           node%element_indices(i))
                        code = code//elem_code
                    end select
                else
                    code = code//"???"
                end if
            else
                code = code//"???"
            end if
        end do
        
        ! Close array constructor based on syntax_style
        if (allocated(node%syntax_style) .and. node%syntax_style == "legacy") then
            code = code//" /)"
        else
            code = code//"]"
        end if
    end function generate_code_array_literal

    ! Generate implied do loop for array constructors
    function generate_implied_do(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, start_code, end_code, step_code
        
        ! Generate the expression (body)
        if (allocated(node%body_indices) .and. size(node%body_indices) > 0) then
            if (node%body_indices(1) > 0 .and. node%body_indices(1) <= arena%size) then
                expr_code = generate_code_from_arena(arena, node%body_indices(1))
            else
                expr_code = "???"
            end if
        else
            expr_code = node%var_name  ! Just the variable if no expression
        end if
        
        ! Generate start expression
        if (node%start_expr_index > 0 .and. node%start_expr_index <= arena%size) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
        else
            start_code = "1"
        end if
        
        ! Generate end expression
        if (node%end_expr_index > 0 .and. node%end_expr_index <= arena%size) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
        else
            end_code = "?"
        end if
        
        ! Build implied do syntax: (expr, var=start,end[,step])
        code = "("//trim(expr_code)//", "//trim(node%var_name)//"="// &
               trim(start_code)//","//trim(end_code)
        
        ! Add step if present
        if (node%step_expr_index > 0 .and. node%step_expr_index <= arena%size) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
            code = code//","//trim(step_code)
        end if
        
        code = code//")"
        
    end function generate_implied_do

    ! Helper function to convert integer to string
    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str
        write (str, '(I0)') num
    end function int_to_string

    ! Helper function to find a node's index in the arena
    function find_node_index_in_arena(arena, target_node) result(index)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: target_node
        integer :: index
        integer :: i

        index = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (same_node(arena%entries(i)%node, target_node)) then
                    index = i
                    return
                end if
            end if
        end do
    end function find_node_index_in_arena

    ! Helper function to compare if two nodes are the same
    function same_node(node1, node2) result(is_same)
        class(ast_node), intent(in) :: node1, node2
        logical :: is_same

        is_same = .false.

        select type (n1 => node1)
        type is (assignment_node)
            select type (n2 => node2)
            type is (assignment_node)
                ! Compare assignment nodes by their target and value indices
                is_same = (n1%target_index == n2%target_index .and. &
                           n1%value_index == n2%value_index)
            end select
        type is (identifier_node)
            select type (n2 => node2)
            type is (identifier_node)
                is_same = (n1%name == n2%name)
            end select
        type is (literal_node)
            select type (n2 => node2)
            type is (literal_node)
               is_same = (n1%value == n2%value .and. n1%literal_kind == n2%literal_kind)
            end select
        end select
    end function same_node

    ! Helper: Check if two declarations can be grouped together
    function can_group_declarations(node1, node2) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        logical :: can_group

        ! Don't group declarations that have initializers
        if (node1%has_initializer .or. node2%has_initializer) then
            can_group = .false.
            return
        end if

        can_group = trim(node1%type_name) == trim(node2%type_name) .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%has_kind .eqv. node2%has_kind .and. &
                    ((node1%has_intent .and. node2%has_intent .and. &
                      trim(node1%intent) == trim(node2%intent)) .or. &
                     (.not. node1%has_intent .and. .not. node2%has_intent)) .and. &
                    node1%is_optional .eqv. node2%is_optional
    end function can_group_declarations

    ! Helper: Check if two parameter declarations can be grouped together
    function can_group_parameters(node1, node2) result(can_group)
        type(parameter_declaration_node), intent(in) :: node1, node2
        logical :: can_group

        can_group = (trim(node1%type_name) == trim(node2%type_name)) .and. &
                    (node1%kind_value == node2%kind_value) .and. &
                    (node1%intent_type == node2%intent_type) .and. &
                    (node1%is_optional .eqv. node2%is_optional)
    end function can_group_parameters

    ! Helper: Build parameter name with array dimensions
    function build_param_name_with_dims(arena, param_node) result(name_with_dims)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: name_with_dims
        integer :: d
        character(len=:), allocatable :: dim_code

        name_with_dims = param_node%name
        if (param_node%is_array .and. allocated(param_node%dimension_indices)) then
            name_with_dims = name_with_dims//"("
            do d = 1, size(param_node%dimension_indices)
                if (d > 1) name_with_dims = name_with_dims//","
             dim_code = generate_code_from_arena(arena, param_node%dimension_indices(d))
                name_with_dims = name_with_dims//dim_code
            end do
            name_with_dims = name_with_dims//")"
        end if
    end function build_param_name_with_dims

    ! Helper: Generate grouped declaration statement
    function generate_grouped_declaration(type_name, kind_value, has_kind, &
                                          intent, var_list, is_optional) result(stmt)
        character(len=*), intent(in) :: type_name
        integer, intent(in) :: kind_value
        logical, intent(in) :: has_kind
        character(len=*), intent(in) :: intent
        character(len=*), intent(in) :: var_list
        logical, intent(in), optional :: is_optional
        character(len=:), allocatable :: stmt
        logical :: opt_flag

        opt_flag = .false.
        if (present(is_optional)) opt_flag = is_optional

        stmt = type_name
        if (has_kind .and. index(type_name, "(") == 0) then
            stmt = stmt//"("//trim(adjustl(int_to_string(kind_value)))//")"
        end if
        if (len_trim(intent) > 0) then
            stmt = stmt//", intent("//intent//")"
        end if
        if (opt_flag) then
            stmt = stmt//", optional"
        end if
        stmt = stmt//" :: "//var_list
    end function generate_grouped_declaration

    ! Generate function/subroutine body with grouped declarations
    function generate_grouped_body(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: indent
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        integer :: i, j, group_start
        logical :: in_declaration_group
        character(len=:), allocatable :: group_type, group_intent, var_list
        integer :: group_kind
        logical :: group_has_kind, group_is_optional

        code = ""
        i = 1

        do while (i <= size(body_indices))
            if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                if (allocated(arena%entries(body_indices(i))%node)) then
                    select type (node => arena%entries(body_indices(i))%node)
                    type is (declaration_node)
                        ! Check if this declaration has initializer
                        if (node%has_initializer .or. node%initializer_index > 0) then
                            ! Don't group declarations with initializers, generate individually
                            stmt_code = generate_code_from_arena(arena, body_indices(i))
                            code = code//indent//stmt_code//new_line('a')
                            i = i + 1
                            cycle
                        end if
                        
                        ! Start of declaration group
                        group_type = node%type_name
                        group_kind = node%kind_value
                        group_has_kind = node%has_kind
                        if (node%has_intent) then
                            group_intent = node%intent
                        else
                            group_intent = ""
                        end if
                        var_list = trim(node%var_name)

                        ! Look ahead for more declarations of same type
                        j = i + 1
                        do while (j <= size(body_indices))
                            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                                if (allocated(arena%entries(body_indices(j))%node)) then
                                    select type (next_node => arena%entries(body_indices(j))%node)
                                    type is (declaration_node)
                                        ! Check if can be grouped
                                        if (can_group_declarations(node, next_node)) then
                                            ! Add to group
                                            var_list = var_list//", "//trim(next_node%var_name)
                                            j = j + 1
                                        else
                                            exit
                                        end if
                                    class default
                                        exit
                                    end select
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        end do

                        ! Generate grouped declaration with optional attribute if present
                        stmt_code = generate_grouped_declaration(group_type, group_kind, &
                                                                 group_has_kind, group_intent, var_list, node%is_optional)

                        code = code//indent//stmt_code//new_line('a')
                        i = j  ! Skip processed declarations

                    type is (parameter_declaration_node)
                        ! Handle parameter declarations
                        ! (convert to regular declarations)
                        group_type = node%type_name
                        group_kind = node%kind_value
                        group_has_kind = (node%kind_value > 0)
                        group_intent = intent_type_to_string(node%intent_type)
                        group_is_optional = node%is_optional

                        ! Build variable name with array specification
                        var_list = build_param_name_with_dims(arena, node)

                        ! Look ahead for more parameter declarations of same type
                        j = i + 1
                        do while (j <= size(body_indices))
                            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                                if (allocated(arena%entries(body_indices(j))%node)) then
                                    select type (next_node => arena%entries(body_indices(j))%node)
                                    type is (parameter_declaration_node)
                                        ! Check if can be grouped
                                        if (can_group_parameters(node, next_node)) then
                                            ! Build next parameter name
                                            ! with array specification
                                            var_list = var_list//", "//build_param_name_with_dims(arena, next_node)
                                            j = j + 1
                                        else
                                            exit
                                        end if
                                    class default
                                        exit
                                    end select
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        end do

                        ! Generate grouped parameter declaration
                        stmt_code = generate_grouped_declaration(group_type, group_kind, &
                                                                 group_has_kind, group_intent, var_list, group_is_optional)

                        code = code//indent//stmt_code//new_line('a')
                        i = j  ! Skip processed parameter declarations

                    class default
                        ! Non-declaration node - generate normally
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code//indent//stmt_code//new_line('a')
                        i = i + 1
                    end select
                else
                    i = i + 1
                end if
            else
                i = i + 1
            end if
        end do

        ! Keep trailing newline - it will be handled by caller if needed
    end function generate_grouped_body

    ! Generate function/subroutine body with grouped declarations and parameter mapping
    function generate_grouped_body_with_params(arena, body_indices, indent, param_map, proc_node) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: indent
        type(parameter_info_t), intent(in) :: param_map(:)
        class(ast_node), intent(in) :: proc_node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        integer :: i, j, group_start, param_idx
        logical :: in_declaration_group
        character(len=:), allocatable :: group_type, group_intent, var_list
        integer :: group_kind
        logical :: group_has_kind, group_is_optional
        character(len=256), allocatable :: param_names(:)
        integer :: param_count, p
        character(len=:), allocatable :: param_list

        code = ""
        
        ! Build parameter name list for filtering individual declarations
        param_count = 0
        
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%param_indices)) param_count = size(proc_node%param_indices)
        type is (subroutine_def_node)  
            if (allocated(proc_node%param_indices)) param_count = size(proc_node%param_indices)
        end select
        
        if (param_count > 0) then
            allocate(param_names(param_count))
            param_count = 0
            
            select type (proc_node)
            type is (function_def_node)
                do p = 1, size(proc_node%param_indices)
                    if (proc_node%param_indices(p) > 0 .and. &
                        proc_node%param_indices(p) <= arena%size) then
                        if (allocated(arena%entries(proc_node%param_indices(p))%node)) then
                            select type (param_node => &
                                         arena%entries(proc_node%param_indices(p))%node)
                            type is (identifier_node)
                                param_count = param_count + 1
                                param_names(param_count) = param_node%name
                            end select
                        end if
                    end if
                end do
            type is (subroutine_def_node)
                do p = 1, size(proc_node%param_indices)
                    if (proc_node%param_indices(p) > 0 .and. &
                        proc_node%param_indices(p) <= arena%size) then
                        if (allocated(arena%entries(proc_node%param_indices(p))%node)) then
                            select type (param_node => &
                                         arena%entries(proc_node%param_indices(p))%node)
                            type is (identifier_node)
                                param_count = param_count + 1
                                param_names(param_count) = param_node%name
                            end select
                        end if
                    end if
                end do
            end select
            
            ! Build parameter list
            param_list = ""
            do p = 1, param_count
                if (len_trim(param_names(p)) > 0) then
                    if (len_trim(param_list) > 0) param_list = param_list//", "
                    param_list = param_list//trim(param_names(p))
                end if
            end do
        else
            allocate(param_names(0))
        end if
        
        ! CRITICAL ARCHITECTURAL FIX: Generate declarations FIRST in correct order
        ! 0. Implicit none FIRST (required Fortran syntax)
        code = code//indent//"implicit none"//new_line('a')
        
        ! 1. Parameter declarations SECOND
        call generate_parameter_declarations_from_semantics(arena, proc_node, indent, code)
        
        ! 2. Result variable declaration THIRD (if present and not explicitly declared)
        call generate_result_variable_declaration_from_semantics(arena, proc_node, indent, code)
        
        ! 3. Process remaining body statements (non-declarations) THIRD
        i = 1

        do while (i <= size(body_indices))
            if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                if (allocated(arena%entries(body_indices(i))%node)) then
                    select type (node => arena%entries(body_indices(i))%node)
                    type is (declaration_node)
                        ! PRESERVE explicit declarations exactly as written
                        ! This is critical for Issue #320 - explicit declarations must be preserved
                        
                        ! Skip result variables ONLY if they're NOT explicitly declared
                        ! If they are explicitly declared, they should be processed here, not by semantic generation
                        select type (proc_node_check => proc_node)
                        type is (function_def_node)
                            if (allocated(proc_node_check%result_variable) .and. &
                                len_trim(proc_node_check%result_variable) > 0 .and. &
                                node%var_name == proc_node_check%result_variable) then
                                ! This is a result variable that's explicitly declared - process it here
                                ! (Semantic generation will skip it because it's explicitly declared)
                            end if
                        end select
                        if (node%is_multi_declaration .and. allocated(node%var_names)) then
                            block
                                logical :: has_parameter, has_non_parameter
                                integer :: k, non_param_count
                                character(len=:), allocatable :: non_param_names(:)
                                
                                has_parameter = .false.
                                has_non_parameter = .false.
                                non_param_count = 0
                                
                                ! Check each variable in the multi-declaration
                                do k = 1, size(node%var_names)
                                    if (is_parameter_name(node%var_names(k), param_names)) then
                                        has_parameter = .true.
                                    else
                                        has_non_parameter = .true.
                                        non_param_count = non_param_count + 1
                                    end if
                                end do
                                
                                if (has_parameter .and. .not. has_non_parameter) then
                                    ! Only parameters - skip entirely
                                    i = i + 1
                                    cycle
                                else if (has_parameter .and. has_non_parameter) then
                                    ! Mixed - filter out parameters and keep only locals
                                    allocate(character(len=256) :: non_param_names(non_param_count))
                                    non_param_count = 0
                                    do k = 1, size(node%var_names)
                                        if (.not. is_parameter_name(node%var_names(k), param_names)) then
                                            non_param_count = non_param_count + 1
                                            non_param_names(non_param_count) = trim(node%var_names(k))
                                        end if
                                    end do
                                    
                                    ! Update var_list with only non-parameters
                                    var_list = ""
                                    do k = 1, non_param_count
                                        if (k > 1) var_list = var_list//", "
                                        var_list = var_list//trim(non_param_names(k))
                                    end do
                                    
                                    if (len_trim(var_list) == 0) then
                                        i = i + 1
                                        cycle
                                    end if
                                end if
                            end block
                        end if
                        
                        ! Start of declaration group
                        group_type = node%type_name
                        group_kind = node%kind_value
                        group_has_kind = node%has_kind
                        
                        ! Check intent and optional - preserve explicit declarations exactly as written
                        block
                            integer :: param_idx
                            param_idx = find_parameter_info(param_map, node%var_name)
                            if (param_idx > 0) then
                                ! This is a parameter that's explicitly declared in the body
                                ! PRESERVE the explicit declaration exactly as written - do NOT override with param_map
                                if (node%has_intent) then
                                    group_intent = node%intent
                                else
                                    group_intent = ""  ! No intent specified in explicit declaration
                                end if
                                group_is_optional = node%is_optional
                            else
                                ! Regular local variable declaration - use node information
                                if (node%has_intent) then
                                    group_intent = node%intent
                                else
                                    group_intent = ""
                                end if
                                group_is_optional = node%is_optional
                            end if
                        end block
                        
                        ! Handle multi-declarations
                        if (node%is_multi_declaration .and. allocated(node%var_names)) then
                            ! Multi-declaration - process each variable
                            var_list = ""
                            block
                                integer :: k
                                do k = 1, size(node%var_names)
                                    if (k > 1) var_list = var_list//", "
                                    var_list = var_list//trim(node%var_names(k))
                                end do
                            end block
                        else
                            var_list = trim(node%var_name)
                        end if

                        ! Look ahead for more declarations of same type
                        j = i + 1
                        do while (j <= size(body_indices))
                            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                                if (allocated(arena%entries(body_indices(j))%node)) then
                                    select type (next_node => arena%entries(body_indices(j))%node)
                                    type is (declaration_node)
                                        ! Check if can be grouped
                                        if (can_group_declarations_with_params(node, next_node, param_map)) then
                                            ! Add to group
                                            var_list = var_list//", "//trim(next_node%var_name)
                                            j = j + 1
                                        else
                                            exit
                                        end if
                                    class default
                                        exit
                                    end select
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        end do

                        ! Generate grouped declaration with parameter attributes if present
                        if (len_trim(var_list) > 0) then
                            stmt_code = generate_grouped_declaration(group_type, group_kind, &
                                                                     group_has_kind, group_intent, var_list, group_is_optional)
                            code = code//indent//stmt_code//new_line('a')
                        end if
                        i = j  ! Skip processed declarations

                    type is (parameter_declaration_node)
                        ! Handle parameter declarations the same as before
                        group_type = node%type_name
                        group_kind = node%kind_value
                        group_has_kind = (node%kind_value > 0)
                        group_intent = intent_type_to_string(node%intent_type)
                        group_is_optional = node%is_optional

                        var_list = build_param_name_with_dims(arena, node)

                        j = i + 1
                        do while (j <= size(body_indices))
                            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                                if (allocated(arena%entries(body_indices(j))%node)) then
                                    select type (next_node => arena%entries(body_indices(j))%node)
                                    type is (parameter_declaration_node)
                                        if (can_group_parameters(node, next_node)) then
                                            var_list = var_list//", "//build_param_name_with_dims(arena, next_node)
                                            j = j + 1
                                        else
                                            exit
                                        end if
                                    class default
                                        exit
                                    end select
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        end do

                        stmt_code = generate_grouped_declaration(group_type, group_kind, &
                                                                 group_has_kind, group_intent, var_list, group_is_optional)

                        code = code//indent//stmt_code//new_line('a')
                    class default
                        ! Generate other statements normally (but avoid recursive declaration generation)
                        if (allocated(arena%entries(body_indices(i))%node)) then
                            select type (check_node => arena%entries(body_indices(i))%node)
                            type is (declaration_node)
                                ! Skip individual declaration nodes to prevent duplication
                                ! They should have been handled by the grouped generation above
                                i = i + 1
                                cycle
                            class default
                                ! Generate non-declaration statements normally
                                stmt_code = generate_code_from_arena(arena, body_indices(i))
                                ! Skip implicit none since we already added it at the beginning
                                if (len_trim(stmt_code) > 0 .and. trim(stmt_code) /= "implicit none") then
                                    code = code//indent//stmt_code//new_line('a')
                                end if
                            end select
                        end if
                        i = i + 1
                    end select
                else
                    i = i + 1
                end if
            else
                i = i + 1
            end if
        end do
        
    end function generate_grouped_body_with_params
    
    ! Find parameter information by name
    function find_parameter_info(param_map, var_name) result(param_idx)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: var_name
        integer :: param_idx
        integer :: i
        
        param_idx = 0
        do i = 1, size(param_map)
            if (allocated(param_map(i)%name) .and. param_map(i)%name == var_name) then
                param_idx = i
                return
            end if
        end do
    end function find_parameter_info
    
    ! Check if a variable name is a procedure parameter
    function is_function_parameter(var_name, arena, proc_node) result(is_param)
        character(len=*), intent(in) :: var_name
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        logical :: is_param
        integer :: i
        
        is_param = .false.
        
        select type (proc_node)
        type is (function_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            
            do i = 1, size(proc_node%param_indices)
                if (proc_node%param_indices(i) > 0 .and. &
                    proc_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(proc_node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(proc_node%param_indices(i))%node)
                        type is (identifier_node)
                            if (param_node%name == var_name) then
                                is_param = .true.
                                return
                            end if
                        end select
                    end if
                end if
            end do
        type is (subroutine_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            
            do i = 1, size(proc_node%param_indices)
                if (proc_node%param_indices(i) > 0 .and. &
                    proc_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(proc_node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(proc_node%param_indices(i))%node)
                        type is (identifier_node)
                            if (param_node%name == var_name) then
                                is_param = .true.
                                return
                            end if
                        end select
                    end if
                end if
            end do
        end select
    end function is_function_parameter
    
    ! Simple helper to check if a name is in the parameter list
    function is_parameter_name(var_name, param_names) result(is_param)
        character(len=*), intent(in) :: var_name
        character(len=256), intent(in) :: param_names(:)
        logical :: is_param
        integer :: i
        
        is_param = .false.
        do i = 1, size(param_names)
            if (trim(param_names(i)) == trim(var_name)) then
                is_param = .true.
                return
            end if
        end do
    end function is_parameter_name
    
    ! Check if declarations can be grouped considering parameter attributes
    function can_group_declarations_with_params(node1, node2, param_map) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        type(parameter_info_t), intent(in) :: param_map(:)
        logical :: can_group
        integer :: param_idx1, param_idx2
        character(len=:), allocatable :: intent1, intent2
        logical :: optional1, optional2
        
        ! Don't group declarations that have initializers
        if (node1%has_initializer .or. node2%has_initializer) then
            can_group = .false.
            return
        end if
        
        ! First check basic grouping criteria
        can_group = can_group_declarations(node1, node2)
        if (.not. can_group) return
        
        ! Get parameter attributes for both variables
        param_idx1 = find_parameter_info(param_map, node1%var_name)
        param_idx2 = find_parameter_info(param_map, node2%var_name)
        
        ! Determine effective intent and optional for node1
        if (param_idx1 > 0) then
            intent1 = param_map(param_idx1)%intent_str
            optional1 = param_map(param_idx1)%is_optional
        else
            if (node1%has_intent) then
                intent1 = node1%intent
            else
                intent1 = ""
            end if
            optional1 = node1%is_optional
        end if
        
        ! Determine effective intent and optional for node2
        if (param_idx2 > 0) then
            intent2 = param_map(param_idx2)%intent_str
            optional2 = param_map(param_idx2)%is_optional
        else
            if (node2%has_intent) then
                intent2 = node2%intent
            else
                intent2 = ""
            end if
            optional2 = node2%is_optional
        end if
        
        ! Check if intent and optional attributes match
        can_group = (intent1 == intent2) .and. (optional1 .eqv. optional2)
    end function can_group_declarations_with_params

    ! Generate code for module node
    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        integer :: i
        
        ! Reset indentation for top-level module
        call reset_indent()
        
        ! Start module
        code = "module "//node%name//new_line('A')
        call increase_indent()
        
        ! Process declarations
        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (node%declaration_indices(i) > 0 .and. &
                    node%declaration_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%declaration_indices(i))%node)) then
                        stmt_code = generate_code_from_arena(arena, &
                                                              node%declaration_indices(i))
                        if (len_trim(stmt_code) > 0) then
                            code = code//with_indent(stmt_code)//new_line('A')
                        end if
                    end if
                end if
            end do
        end if
        
        ! Process procedures
        if (allocated(node%procedure_indices) .and. &
            size(node%procedure_indices) > 0) then
            ! Add contains statement
            code = code//"contains"//new_line('A')
            
            ! Generate procedures
            do i = 1, size(node%procedure_indices)
                if (node%procedure_indices(i) > 0 .and. &
                    node%procedure_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%procedure_indices(i))%node)) then
                        stmt_code = generate_code_from_arena(arena, &
                                                              node%procedure_indices(i))
                        if (len_trim(stmt_code) > 0) then
                            code = code//with_indent(stmt_code)//new_line('A')
                        end if
                    end if
                end if
            end do
        end if
        
        call decrease_indent()
        code = code//"end module "//node%name
    end function generate_code_module

    ! Generate code for comment node
    function generate_code_comment(node) result(code)
        type(comment_node), intent(in) :: node
        character(len=:), allocatable :: code

        if (allocated(node%text)) then
            code = node%text  ! Comments are output as-is
        else
            code = "!"
        end if
    end function generate_code_comment

    ! Generate code for implicit statement
    function generate_code_implicit_statement(node) result(code)
        type(implicit_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=256) :: temp_str
        
        if (node%is_none) then
            code = "implicit none"
        else
            ! Build type specification
            code = "implicit "
            
            if (allocated(node%type_spec%type_name)) then
                code = code // node%type_spec%type_name
                
                ! Add kind or length specification
                if (node%type_spec%has_kind .or. node%type_spec%has_length) then
                    code = code // "("
                    
                    if (node%type_spec%has_length) then
                        ! Character length specification
                        if (node%type_spec%length_value > 0) then
                            write(temp_str, '(I0)') node%type_spec%length_value
                            code = code // "len=" // trim(temp_str)
                        end if
                    else if (node%type_spec%has_kind) then
                        ! Kind specification
                        if (node%type_spec%kind_value > 0) then
                            write(temp_str, '(I0)') node%type_spec%kind_value
                            code = code // trim(temp_str)
                        end if
                    end if
                    
                    code = code // ")"
                end if
            end if
            
            ! Add letter specifications
            if (allocated(node%letter_specs) .and. size(node%letter_specs) > 0) then
                code = code // " ("
                
                do i = 1, size(node%letter_specs)
                    if (i > 1) code = code // ", "
                    
                    code = code // node%letter_specs(i)%start_letter
                    if (node%letter_specs(i)%start_letter /= node%letter_specs(i)%end_letter) then
                        code = code // "-" // node%letter_specs(i)%end_letter
                    end if
                end do
                
                code = code // ")"
            end if
        end if
    end function generate_code_implicit_statement

    ! Set type standardization configuration
    subroutine set_type_standardization(enabled)
        logical, intent(in) :: enabled
        standardize_types_enabled = enabled
    end subroutine set_type_standardization

    ! Get current type standardization configuration
    subroutine get_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardize_types_enabled
    end subroutine get_type_standardization

    ! Generate code for range expression node
    function generate_code_range_expression(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_expression_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, stride_code
        
        code = ""
        
        ! Generate start bound
        if (node%start_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_index)
        else
            start_code = ""
        end if
        
        ! Generate end bound
        if (node%end_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_index)
        else
            end_code = ""
        end if
        
        ! Generate stride if present
        if (node%stride_index > 0) then
            stride_code = generate_code_from_arena(arena, node%stride_index)
            code = start_code // ":" // end_code // ":" // stride_code
        else
            code = start_code // ":" // end_code
        end if
    end function generate_code_range_expression

    ! Generate code for array bounds node
    function generate_code_array_bounds(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_bounds_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: lower_code, upper_code, stride_code
        
        code = ""
        
        ! Handle special cases
        if (node%is_assumed_shape) then
            code = ":"
            return
        else if (node%is_assumed_size) then
            code = "*"
            return
        else if (node%is_deferred_shape) then
            code = ":"
            return
        end if
        
        ! Generate lower bound
        if (node%lower_bound_index > 0) then
            lower_code = generate_code_from_arena(arena, node%lower_bound_index)
        else
            lower_code = ""
        end if
        
        ! Generate upper bound
        if (node%upper_bound_index > 0) then
            upper_code = generate_code_from_arena(arena, node%upper_bound_index)
        else
            upper_code = ""
        end if
        
        ! Generate stride if present
        if (node%stride_index > 0) then
            stride_code = generate_code_from_arena(arena, node%stride_index)
            code = lower_code // ":" // upper_code // ":" // stride_code
        else
            code = lower_code // ":" // upper_code
        end if
    end function generate_code_array_bounds

    ! Generate code for array slice node
    function generate_code_array_slice(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: array_code, bounds_code
        integer :: i
        
        ! Generate array expression
        if (node%array_index > 0) then
            array_code = generate_code_from_arena(arena, node%array_index)
        else
            array_code = "!error_array"
        end if
        
        code = array_code // "("
        
        ! Generate bounds for each dimension
        do i = 1, node%num_dimensions
            if (i > 1) code = code // ", "
            if (node%bounds_indices(i) > 0) then
                bounds_code = generate_code_from_arena(arena, node%bounds_indices(i))
                code = code // bounds_code
            else
                code = code // ":"  ! Empty bounds
            end if
        end do
        
        code = code // ")"
    end function generate_code_array_slice

    ! Generate code for array operation node
    function generate_code_array_operation(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_operation_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code
        
        ! Generate code for operands
        if (node%left_operand_index > 0) then
            left_code = generate_code_from_arena(arena, node%left_operand_index)
        else
            left_code = "? "
        end if
        
        if (node%right_operand_index > 0) then
            right_code = generate_code_from_arena(arena, node%right_operand_index)
        else
            right_code = " ?"
        end if
        
        ! Generate the operation
        select case (trim(node%operation))
        case ("=")
            code = left_code//" = "//right_code
        case ("+")
            code = left_code//" + "//right_code
        case ("-")
            code = left_code//" - "//right_code
        case ("*")
            code = left_code//" * "//right_code
        case ("/")
            code = left_code//" / "//right_code
        case ("**")
            code = left_code//" ** "//right_code
        case default
            code = left_code//" "//trim(node%operation)//" "//right_code
        end select
        
        ! Add bounds checking comments if available
        if (node%bounds_checked) then
            code = code//" ! bounds checked"
        end if
        
        if (node%shape_conformant) then
            code = code//" ! shape conformant"
        end if
    end function generate_code_array_operation

    ! Generate code for derived type definition  
    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: component_code
        integer :: i

        code = ""
        
        ! Generate type header
        code = "type :: "//node%name
        
        ! Generate components
        if (allocated(node%component_indices)) then
            do i = 1, size(node%component_indices)
                if (node%component_indices(i) > 0 .and. &
                    node%component_indices(i) <= arena%size) then
                    component_code = generate_code_from_arena(arena, node%component_indices(i))
                    if (len_trim(component_code) > 0) then
                        code = code//new_line('a')//"    "//component_code
                    end if
                end if
            end do
        end if
        
        ! Generate type footer
        code = code//new_line('a')//"end type"
        
        ! Add type name after 'end type' if it exists  
        if (len_trim(node%name) > 0) then
            code = code//" "//node%name
        end if
    end function generate_code_derived_type

    ! Add line continuations when line exceeds configured length
    function add_line_continuations(input_code) result(output_code)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable :: output_code
        integer :: line_length_limit
        
        ! Get the configured line length limit
        call get_line_length_config(line_length_limit)
        
        ! Apply line continuation if the input exceeds the limit
        if (len(input_code) > line_length_limit) then
            call add_line_with_continuation(input_code, line_length_limit, &
                                          output_code)
        else
            output_code = input_code
        end if
    end function add_line_continuations
    
    ! Helper subroutine to break a long line with continuation characters
    subroutine add_line_with_continuation(long_line, max_length, output_code)
        character(len=*), intent(in) :: long_line
        integer, intent(in) :: max_length
        character(len=:), allocatable, intent(inout) :: output_code
        integer :: break_pos
        character(len=:), allocatable :: continuation_indent
        
        ! Handle lines that fit within the limit
        if (len(long_line) <= max_length) then
            output_code = long_line
            return
        end if
        
        ! Create continuation indentation (current indent + 9 spaces)
        continuation_indent = get_indent()//"         "
        
        ! Find a reasonable break point near max_length
        break_pos = max_length - 5  ! Leave room for " &"
        if (break_pos > 1 .and. break_pos < len(long_line)) then
            ! Look backwards for good break point
            do while (break_pos > max_length/2 .and. break_pos > 1)
                if (long_line(break_pos:break_pos) == ' ' .or. &
                    long_line(break_pos:break_pos) == '+' .or. &
                    long_line(break_pos:break_pos) == '-') then
                    exit  ! Good break point found
                end if
                break_pos = break_pos - 1
            end do
        end if
        
        ! If no good break point found, use max_length - 5
        if (break_pos <= max_length/2) then
            break_pos = max_length - 5
        end if
        
        ! Create the broken line with continuation
        output_code = trim(long_line(1:break_pos))//" &"//new_line('a')// &
                      continuation_indent//trim(long_line(break_pos+1:))
    end subroutine add_line_with_continuation

    ! Safe version of generate_code_from_arena that uses subroutine interface
    ! to avoid problematic allocatable string assignments
    subroutine safe_generate_code_from_arena(arena, node_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        
        ! Use a local block to contain any potential crashes
        block
            character(len=:), allocatable :: temp_code
            
            ! Call the original function but handle any errors
            temp_code = generate_code_from_arena(arena, node_index)
            
            ! Safely assign the result
            if (allocated(temp_code)) then
                code = temp_code
            else
                code = "! Error: Code generation failed"
            end if
        end block
        
    end subroutine safe_generate_code_from_arena

    ! Generate parameter declarations using semantic type information
    ! Check if a variable is already explicitly declared in the function body
    logical function is_variable_explicitly_declared(arena, proc_node, var_name)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        character(len=*), intent(in) :: var_name
        
        integer :: i, body_index
        
        is_variable_explicitly_declared = .false.
        
        ! Check if variable is explicitly declared in function body
        
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%body_indices)) then
                ! Check each body node for explicit declarations
                do i = 1, size(proc_node%body_indices)
                    body_index = proc_node%body_indices(i)
                    if (body_index > 0 .and. body_index <= arena%size) then
                        if (allocated(arena%entries(body_index)%node)) then
                            select type (body_node => arena%entries(body_index)%node)
                            type is (declaration_node)
                                ! Check if this declaration matches the variable
                                if (body_node%var_name == var_name) then
                                    ! Found explicit declaration for this variable
                                    is_variable_explicitly_declared = .true.
                                    return
                                end if
                                ! Check multi-variable declarations
                                if (body_node%is_multi_declaration .and. allocated(body_node%var_names)) then
                                    block
                                        integer :: j
                                        do j = 1, size(body_node%var_names)
                                            if (body_node%var_names(j) == var_name) then
                                                is_variable_explicitly_declared = .true.
                                                return
                                            end if
                                        end do
                                    end block
                                end if
                            type is (parameter_declaration_node)
                                ! DEBUG: Uncomment for debugging
                                ! print *, "DEBUG: Found parameter_declaration_node for: '", trim(body_node%name), "'"
                                if (body_node%name == var_name) then
                                    is_variable_explicitly_declared = .true.
                                    return
                                end if
                            class default
                                ! Other node types - continue searching
                            end select
                        end if
                    end if
                end do
            end if
        type is (subroutine_def_node)
            if (allocated(proc_node%body_indices)) then
                do i = 1, size(proc_node%body_indices)
                    body_index = proc_node%body_indices(i)
                    if (body_index > 0 .and. body_index <= arena%size) then
                        if (allocated(arena%entries(body_index)%node)) then
                            select type (body_node => arena%entries(body_index)%node)
                            type is (declaration_node)
                                if (body_node%var_name == var_name) then
                                    is_variable_explicitly_declared = .true.
                                    return
                                end if
                                ! Check multi-variable declarations
                                if (body_node%is_multi_declaration .and. allocated(body_node%var_names)) then
                                    block
                                        integer :: j
                                        do j = 1, size(body_node%var_names)
                                            if (body_node%var_names(j) == var_name) then
                                                is_variable_explicitly_declared = .true.
                                                return
                                            end if
                                        end do
                                    end block
                                end if
                            type is (parameter_declaration_node)
                                if (body_node%name == var_name) then
                                    is_variable_explicitly_declared = .true.
                                    return
                                end if
                            end select
                        end if
                    end if
                end do
            end if
        end select
        
        ! Variable not found as explicitly declared
    end function is_variable_explicitly_declared

    subroutine generate_parameter_declarations_from_semantics(arena, proc_node, indent, code)
        use type_system_hm, only: mono_type_t, TVAR
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        character(len=*), intent(in) :: indent
        character(len=:), allocatable, intent(inout) :: code
        
        ! Parameter grouping support
        type :: parameter_info_t
            character(len=:), allocatable :: name
            character(len=:), allocatable :: type_str
            character(len=:), allocatable :: intent_str
        end type parameter_info_t
        
        integer :: i, param_index, group_start, group_end, j, outer_loop_counter, inner_loop_counter
        character(len=:), allocatable :: param_type_str, param_name, intent_str
        type(parameter_info_t), allocatable :: param_info(:)
        character(len=:), allocatable :: param_list, current_type_intent
        integer :: param_count
        
        ! Generate parameter declarations with grouping by type and intent
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%param_indices)) then
                ! First pass: collect parameter information
                param_count = 0
                allocate(param_info(size(proc_node%param_indices)))
                
                do i = 1, size(proc_node%param_indices)
                    param_index = proc_node%param_indices(i)
                    if (param_index > 0 .and. param_index <= arena%size) then
                        if (allocated(arena%entries(param_index)%node)) then
                            ! Get parameter name and intent information
                            select type (param_node => arena%entries(param_index)%node)
                            type is (identifier_node)
                                param_name = param_node%name
                                
                                ! Check if parameter is already explicitly declared - if so, skip auto-generation
                                if (is_variable_explicitly_declared(arena, proc_node, param_name)) then
                                    cycle  ! Skip this parameter, it's already explicitly declared
                                end if
                                
                                intent_str = "intent(in)"  ! Default for identifier parameters
                                
                                ! Get type from semantic analysis
                                if (allocated(param_node%inferred_type)) then
                                    param_type_str = param_node%inferred_type%to_string()
                                    ! Convert semantic type to Fortran declaration format
                                    if (param_type_str == "real(8)") then
                                        param_type_str = "real(8)"
                                    else if (param_type_str == "integer") then
                                        param_type_str = "integer"
                                    else if (index(param_type_str, "character") > 0) then
                                        ! Keep character types as-is from semantic analysis
                                        param_type_str = param_type_str
                                    else if (param_node%inferred_type%kind == TVAR) then
                                        ! Type variable - apply Fortran implicit typing rules
                                        param_type_str = apply_implicit_typing_rules(param_name)
                                    else
                                        ! Fallback for unknown types - apply implicit typing rules
                                        param_type_str = apply_implicit_typing_rules(param_name)
                                    end if
                                else
                                    ! No semantic type information - apply Fortran implicit typing rules
                                    param_type_str = apply_implicit_typing_rules(param_name)
                                end if
                                
                                ! Store parameter information
                                param_count = param_count + 1
                                param_info(param_count)%name = param_name
                                param_info(param_count)%type_str = param_type_str
                                param_info(param_count)%intent_str = intent_str
                            type is (parameter_declaration_node)
                                param_name = param_node%name
                                
                                ! Check if parameter is already explicitly declared - if so, skip auto-generation
                                if (is_variable_explicitly_declared(arena, proc_node, param_name)) then
                                    cycle  ! Skip this parameter, it's already explicitly declared
                                end if
                                
                                ! Get proper intent from parameter declaration
                                intent_str = "intent("//intent_type_to_string(param_node%intent_type)//")"
                                if (param_node%intent_type == INTENT_NONE) then
                                    intent_str = "intent(in)"  ! Default if no intent specified
                                end if
                                
                                ! Get type from parameter declaration or apply implicit typing
                                if (allocated(param_node%type_name) .and. len_trim(param_node%type_name) > 0) then
                                    param_type_str = param_node%type_name
                                else
                                    ! Apply implicit typing rules
                                    param_type_str = apply_implicit_typing_rules(param_name)
                                end if
                                
                                ! Store parameter information
                                param_count = param_count + 1
                                param_info(param_count)%name = param_name
                                param_info(param_count)%type_str = param_type_str
                                param_info(param_count)%intent_str = intent_str
                            end select
                        end if
                    end if
                end do
                
                ! Second pass: generate grouped declarations
                if (param_count > 0) then
                    ! TEMPORARY FIX: Skip parameter grouping to prevent infinite loop
                    ! Generate each parameter declaration individually instead
                    do i = 1, param_count
                        code = code//indent//param_info(i)%type_str//" :: "//param_info(i)%name//new_line('a')
                    end do
                else
                    ! Original grouped logic (keeping as backup but disabled)
                    i = 1
                    outer_loop_counter = 0
                    do while (i <= param_count)
                        outer_loop_counter = outer_loop_counter + 1
                        if (outer_loop_counter > 1000) exit  ! Prevent outer loop infinite loop
                        
                        ! Start a new group
                        current_type_intent = param_info(i)%type_str // ", " // param_info(i)%intent_str
                        param_list = param_info(i)%name
                        
                        ! Find all parameters with same type and intent
                        j = i + 1
                        inner_loop_counter = 0
                        do while (j <= param_count .and. inner_loop_counter < 1000)
                            inner_loop_counter = inner_loop_counter + 1
                            if (param_info(j)%type_str // ", " // param_info(j)%intent_str == current_type_intent) then
                                param_list = param_list // ", " // param_info(j)%name
                                ! Remove this parameter from further consideration
                                do group_end = j, param_count - 1
                                    param_info(group_end) = param_info(group_end + 1)
                                end do
                                param_count = param_count - 1
                                ! NOTE: Don't increment j here because the next element has shifted into position j
                            else
                                j = j + 1
                            end if
                        end do
                        
                        ! Generate grouped declaration
                        code = code//indent//current_type_intent//" :: "//param_list//new_line('a')
                        i = i + 1
                    end do
                end if  ! End original grouped logic
            end if
        type is (subroutine_def_node)
            if (allocated(proc_node%param_indices)) then
                do i = 1, size(proc_node%param_indices)
                    param_index = proc_node%param_indices(i)
                    if (param_index > 0 .and. param_index <= arena%size) then
                        if (allocated(arena%entries(param_index)%node)) then
                            ! Get parameter name and intent information
                            select type (param_node => arena%entries(param_index)%node)
                            type is (identifier_node)
                                param_name = param_node%name
                                
                                ! Check if parameter is already explicitly declared - if so, skip auto-generation
                                if (is_variable_explicitly_declared(arena, proc_node, param_name)) then
                                    cycle  ! Skip this parameter, it's already explicitly declared
                                end if
                                
                                intent_str = "intent(in)"  ! Default for identifier parameters
                                
                                ! Get type from semantic analysis
                                if (allocated(param_node%inferred_type)) then
                                    param_type_str = param_node%inferred_type%to_string()
                                    ! Convert semantic type to Fortran declaration format
                                    if (param_type_str == "real(8)") then
                                        param_type_str = "real(8)"
                                    else if (param_type_str == "integer") then
                                        param_type_str = "integer"
                                    else if (index(param_type_str, "character") > 0) then
                                        param_type_str = param_type_str
                                    else if (param_node%inferred_type%kind == TVAR) then
                                        param_type_str = apply_implicit_typing_rules(param_name)
                                    else
                                        param_type_str = apply_implicit_typing_rules(param_name)
                                    end if
                                else
                                    param_type_str = apply_implicit_typing_rules(param_name)
                                end if
                                
                                ! Generate parameter declaration (subroutines can have intent(in), intent(out), intent(inout))
                                code = code//indent//param_type_str//", "//intent_str//" :: "//param_name//new_line('a')
                            type is (parameter_declaration_node)
                                param_name = param_node%name
                                
                                ! Check if parameter is already explicitly declared - if so, skip auto-generation
                                if (is_variable_explicitly_declared(arena, proc_node, param_name)) then
                                    cycle  ! Skip this parameter, it's already explicitly declared
                                end if
                                
                                ! Get proper intent from parameter declaration
                                intent_str = "intent("//intent_type_to_string(param_node%intent_type)//")"
                                if (param_node%intent_type == INTENT_NONE) then
                                    intent_str = "intent(in)"  ! Default if no intent specified
                                end if
                                
                                ! Get type from parameter declaration or apply implicit typing
                                if (allocated(param_node%type_name) .and. len_trim(param_node%type_name) > 0) then
                                    param_type_str = param_node%type_name
                                else
                                    ! Apply implicit typing rules
                                    param_type_str = apply_implicit_typing_rules(param_name)
                                end if
                                
                                ! Generate parameter declaration with proper intent
                                ! (subroutines can have intent(in), intent(out), intent(inout))
                                code = code//indent//param_type_str//", "//intent_str//" :: "//param_name//new_line('a')
                            end select
                        end if
                    end if
                end do
            end if
        end select
    end subroutine generate_parameter_declarations_from_semantics

    ! Generate result variable declaration using semantic type information
    subroutine generate_result_variable_declaration_from_semantics(arena, proc_node, indent, code)
        use type_system_hm, only: mono_type_t, TVAR
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        character(len=*), intent(in) :: indent
        character(len=:), allocatable, intent(inout) :: code
        
        character(len=:), allocatable :: result_type_str, result_name
        logical :: result_declared_explicitly
        integer :: i, body_index
        
        
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%result_variable) .and. len_trim(proc_node%result_variable) > 0) then
                result_name = proc_node%result_variable
                
                ! Check if result variable is already explicitly declared - if so, skip auto-generation
                if (is_variable_explicitly_declared(arena, proc_node, result_name)) then
                    return  ! Skip this result variable, it's already explicitly declared
                end if
                
                ! Generate result variable declaration - only if not explicitly declared
                ! Try to infer type from assignments, or use default
                result_type_str = "real(8)"  ! Default fallback
                
                ! Look for assignment nodes to infer the result type
                if (allocated(proc_node%body_indices)) then
                    do i = 1, size(proc_node%body_indices)
                        body_index = proc_node%body_indices(i)
                        if (body_index > 0 .and. body_index <= arena%size) then
                            if (allocated(arena%entries(body_index)%node)) then
                                select type (body_node => arena%entries(body_index)%node)
                                type is (assignment_node)
                                    ! Check if this is assignment to result variable
                                    if (body_node%target_index > 0 .and. body_node%target_index <= arena%size) then
                                        if (allocated(arena%entries(body_node%target_index)%node)) then
                                            select type (target_node => arena%entries(body_node%target_index)%node)
                                            type is (identifier_node)
                                                if (target_node%name == result_name) then
                                                    ! This assigns to result variable - use RHS type
                                                    if (body_node%value_index > 0 .and. body_node%value_index <= arena%size) then
                                                        if (allocated(arena%entries(body_node%value_index)%node)) then
                                                            if (allocated(arena%entries(body_node%value_index)%node% &
                                                                    inferred_type)) then
                                                                result_type_str = arena%entries(body_node%value_index)% &
                                                                        node%inferred_type%to_string()
                                                                
                                                                ! Convert semantic type to Fortran declaration format
                                                                if (result_type_str == "real(8)") then
                                                                    result_type_str = "real(8)"
                                                                else if (result_type_str == "integer") then
                                                                    result_type_str = "integer"
                                                                else if (index(result_type_str, "character") > 0) then
                                                                    result_type_str = result_type_str
                                                                else
                                                                    result_type_str = "real(8)"  ! Default fallback
                                                                end if
                                                                exit  ! Found type, no need to continue searching
                                                            end if
                                                        end if
                                                    end if
                                                end if
                                            end select
                                        end if
                                    end if
                                end select
                            end if
                        end if
                    end do
                end if
                
                ! Generate result variable declaration
                code = code//indent//result_type_str//" :: "//result_name//new_line('a')
            end if
        end select
    end subroutine generate_result_variable_declaration_from_semantics

    ! Apply Fortran implicit typing rules for variable names
    function apply_implicit_typing_rules(var_name) result(type_str)
        character(len=*), intent(in) :: var_name
        character(len=:), allocatable :: type_str
        
        character :: first_char
        
        
        if (len_trim(var_name) == 0) then
            type_str = "real(8)"  ! Fallback for empty names
            return
        end if
        
        ! Get first character and convert to lowercase
        first_char = var_name(1:1)
        if (first_char >= 'A' .and. first_char <= 'Z') then
            first_char = char(ichar(first_char) + 32)  ! Convert to lowercase
        end if
        
        ! Apply Fortran implicit typing rules
        ! Variables starting with i, j, k, l, m, n are integer
        ! All others are real
        if (first_char == 'i' .or. first_char == 'j' .or. first_char == 'k' .or. &
            first_char == 'l' .or. first_char == 'm' .or. first_char == 'n') then
            type_str = "integer"
        else
            type_str = "real(8)"
        end if
        
    end function apply_implicit_typing_rules

end module codegen_core
