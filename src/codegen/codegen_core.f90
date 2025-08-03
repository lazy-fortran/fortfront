module codegen_core
    use ast_core
    use ast_nodes_core, only: component_access_node, range_subscript_node
    use ast_nodes_control, only: associate_node
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE
    use type_system_hm
    use string_types, only: string_t
    use codegen_indent
    implicit none
    private

    ! Type for storing parameter information during codegen
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
    public :: generate_code_from_arena, generate_code_polymorphic
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
        type is (array_literal_node)
            code = generate_code_array_literal(arena, node, node_index)
        type is (stop_node)
            code = generate_code_stop(arena, node, node_index)
        type is (return_node)
            code = generate_code_return(arena, node, node_index)
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
    end function generate_code_assignment

    ! Generate code for binary operation node
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate code for operands
        if (node%left_index > 0 .and. node%left_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%left_index)
        else
            left_code = ""
        end if

        if (node%right_index > 0 .and. node%right_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%right_index)
        else
            right_code = ""
        end if

        ! Combine with operator - match fprettify spacing rules
        ! fprettify: * and / get no spaces, +/- and comparisons get spaces
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
        else if (trim(node%operator) == '*' .or. trim(node%operator) == '/') then
            code = left_code//node%operator//right_code
        else
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
                            if (len(code) > 0) then
                                code = code//new_line('A')
                            end if
                            code = code//stmt_code
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
                            if (len(declarations) > 0) then
                                declarations = declarations//new_line('A')
                            end if
                            declarations = declarations//"implicit none"
                            has_declarations = .true.
                            has_implicit_none = .true.
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
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = ""
                            param_map(i)%is_optional = .false.
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration has intent and matches a parameter
                                print *, "DEBUG: Found declaration_node for ", trim(body_node%var_name), &
                                       " has_intent=", body_node%has_intent, &
                                       " is_optional=", body_node%is_optional
                                if (body_node%has_intent) then
                                    print *, "DEBUG: Intent value: ", trim(body_node%intent)
                                    do i = 1, param_count
                                        if (allocated(param_map(i)%name) .and. &
                                            param_map(i)%name == body_node%var_name) then
                                            param_map(i)%intent_str = body_node%intent
                                            param_map(i)%is_optional = body_node%is_optional
                                            print *, "DEBUG: Updated param ", trim(param_map(i)%name), &
                                                   " with intent=", trim(param_map(i)%intent_str), &
                                                   " optional=", param_map(i)%is_optional
                                        end if
                                    end do
                                end if
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code//generate_grouped_body_with_params(arena, node%body_indices, "    ", param_map)
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
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = ""
                            param_map(i)%is_optional = .false.
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration has intent and matches a parameter
                                print *, "DEBUG: Found declaration_node for ", trim(body_node%var_name), &
                                       " has_intent=", body_node%has_intent, &
                                       " is_optional=", body_node%is_optional
                                if (body_node%has_intent) then
                                    print *, "DEBUG: Intent value: ", trim(body_node%intent)
                                    do i = 1, param_count
                                        if (allocated(param_map(i)%name) .and. &
                                            param_map(i)%name == body_node%var_name) then
                                            param_map(i)%intent_str = body_node%intent
                                            param_map(i)%is_optional = body_node%is_optional
                                            print *, "DEBUG: Updated param ", trim(param_map(i)%name), &
                                                   " with intent=", trim(param_map(i)%intent_str), &
                                                   " optional=", param_map(i)%is_optional
                                        end if
                                    end do
                                end if
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code//generate_grouped_body_with_params(arena, node%body_indices, "    ", param_map)
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
        integer :: i, j
        
        ! Build index specifications
        index_spec = ""
        do i = 1, node%num_indices
            if (i > 1) index_spec = index_spec//", "
            
            ! Add index name
            if (allocated(node%index_names)) then
                index_spec = index_spec//trim(node%index_names(i))//"="
            else
                index_spec = index_spec//"i"//trim(adjustl(int_to_string(i)))//"="
            end if
            
            ! Add lower bound
            if (allocated(node%lower_bound_indices)) then
                if (node%lower_bound_indices(i) > 0 .and. &
                    node%lower_bound_indices(i) <= arena%size) then
                    index_spec = index_spec// &
                        generate_code_from_arena(arena, node%lower_bound_indices(i))
                else
                    index_spec = index_spec//"1"
                end if
            else
                index_spec = index_spec//"1"
            end if
            
            index_spec = index_spec//":"
            
            ! Add upper bound
            if (allocated(node%upper_bound_indices)) then
                if (node%upper_bound_indices(i) > 0 .and. &
                    node%upper_bound_indices(i) <= arena%size) then
                    index_spec = index_spec// &
                        generate_code_from_arena(arena, node%upper_bound_indices(i))
                else
                    index_spec = index_spec//"n"
                end if
            else
                index_spec = index_spec//"n"
            end if
            
            ! Add optional stride
            if (allocated(node%stride_indices)) then
                if (node%stride_indices(i) > 0 .and. &
                    node%stride_indices(i) <= arena%size) then
                    index_spec = index_spec//":"// &
                        generate_code_from_arena(arena, node%stride_indices(i))
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
            
            do j = 1, size(node%body_indices)
                if (node%body_indices(j) > 0 .and. &
                    node%body_indices(j) <= arena%size) then
                    stmt_code = generate_code_from_arena(arena, node%body_indices(j))
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
        integer :: i

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
                if (node%inferred_type%size > 0) then
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
        if (node%has_kind .and. node%type_name /= "character") then
            code = code//"("//trim(adjustl(int_to_string(node%kind_value)))//")"
        else if (node%type_name == "character" .and. node%has_kind) then
            ! For character, kind_value is actually the length
            code = "character(len="//trim(adjustl(int_to_string(node%kind_value)))//")"
        end if

        ! Add intent if present
        if (node%has_intent .and. allocated(node%intent)) then
            code = code//", intent("//node%intent//")"
        end if

        ! Add allocatable if present
        if (node%is_allocatable) then
            code = code//", allocatable"
        end if

        ! Add variable names - handle both single and multi declarations
        code = code//" :: "
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            ! Multi-variable declaration
            do i = 1, size(node%var_names)
                if (i > 1) code = code//", "
                code = code//trim(node%var_names(i))
            end do
        else
            ! Single variable declaration
            code = code//node%var_name
        end if

        ! Add array dimensions if present
        if (node%is_array .and. allocated(node%dimension_indices)) then
            ! Generate dimension expressions
            code = code//"("
            do i = 1, size(node%dimension_indices)
                if (i > 1) code = code//","
   if (node%dimension_indices(i) > 0 .and. node%dimension_indices(i) <= arena%size) then
                 code = code//generate_code_from_arena(arena, node%dimension_indices(i))
                else
                    code = code//":"  ! Default for unspecified dimensions
                end if
            end do
            code = code//")"
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

        ! Check if we have elements
        if (.not. allocated(node%element_indices) .or. &
            size(node%element_indices) == 0) then
            ! Empty array - use Fortran 2003 syntax
            code = "[integer ::]"
            return
        end if

        ! Generate array constructor
        code = "(/ "
        
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
        
        code = code//" /)"
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

        can_group = node1%type_name == node2%type_name .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%has_kind .eqv. node2%has_kind .and. &
                    ((node1%has_intent .and. node2%has_intent .and. &
                      node1%intent == node2%intent) .or. &
                     (.not. node1%has_intent .and. .not. node2%has_intent)) .and. &
                    node1%is_optional .eqv. node2%is_optional
    end function can_group_declarations

    ! Helper: Check if two parameter declarations can be grouped together
    function can_group_parameters(node1, node2) result(can_group)
        type(parameter_declaration_node), intent(in) :: node1, node2
        logical :: can_group

        can_group = node1%type_name == node2%type_name .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%intent_type == node2%intent_type .and. &
                    node1%is_optional .eqv. node2%is_optional
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
        if (has_kind) then
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
    function generate_grouped_body_with_params(arena, body_indices, indent, param_map) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: indent
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        integer :: i, j, group_start, param_idx
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
                        ! Start of declaration group
                        group_type = node%type_name
                        group_kind = node%kind_value
                        group_has_kind = node%has_kind
                        
                        ! Check if this variable is a parameter
                        param_idx = find_parameter_info(param_map, node%var_name)
                        if (param_idx > 0) then
                            ! Use parameter attributes
                            group_intent = param_map(param_idx)%intent_str
                            group_is_optional = param_map(param_idx)%is_optional
                        else
                            ! Use declaration attributes
                            if (node%has_intent) then
                                group_intent = node%intent
                            else
                                group_intent = ""
                            end if
                            group_is_optional = node%is_optional
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
                      stmt_code = generate_grouped_declaration(group_type, group_kind, &
                                                 group_has_kind, group_intent, var_list, group_is_optional)

                        code = code//indent//stmt_code//new_line('a')
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
                        i = j

                    class default
                        ! Generate other statements normally
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        if (len_trim(stmt_code) > 0) then
                            code = code//indent//stmt_code//new_line('a')
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
    
    ! Check if declarations can be grouped considering parameter attributes
    function can_group_declarations_with_params(node1, node2, param_map) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        type(parameter_info_t), intent(in) :: param_map(:)
        logical :: can_group
        integer :: param_idx1, param_idx2
        character(len=:), allocatable :: intent1, intent2
        logical :: optional1, optional2
        
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
                if (node%declaration_indices(i) > 0) then
                    stmt_code = generate_code_from_arena(arena, &
                                                          node%declaration_indices(i))
                    if (len_trim(stmt_code) > 0) then
                        code = code//with_indent(stmt_code)//new_line('A')
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
                if (node%procedure_indices(i) > 0) then
                    stmt_code = generate_code_from_arena(arena, &
                                                          node%procedure_indices(i))
                    if (len_trim(stmt_code) > 0) then
                        code = code//with_indent(stmt_code)//new_line('A')
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

end module codegen_core
