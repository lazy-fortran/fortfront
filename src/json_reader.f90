module json_reader
    ! JSON deserialization for frontend stages
    ! Reads JSON files and reconstructs tokens, AST nodes, etc.

    use json_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, TK_NUMBER, TK_STRING, TK_NEWLINE, TK_EOF
    use ast_core
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use string_types, only: string_t
    ! Note: Using core AST nodes only - no dialect-specific imports
    implicit none
    private

    ! Public interface
    public :: json_read_tokens_from_file, json_read_ast_from_file
    public :: json_read_semantic_from_file
    public :: json_to_tokens, json_to_ast, json_to_semantic

contains

    ! Read tokens from JSON file
    function json_read_tokens_from_file(filename) result(tokens)
        character(len=*), intent(in) :: filename
        type(token_t), allocatable :: tokens(:)
        type(json_file) :: json
        logical :: found, status_ok
        character(len=:), allocatable :: error_msg

        ! Load JSON file
        call json%load(filename=filename)

        ! Check for JSON parsing errors - safely handle potential issues
        block
            logical :: json_valid
            json_valid = .true.

            ! Try to get the root - if this fails, JSON is invalid
            block
                type(json_value), pointer :: root_test
                call json%get(root_test)
                if (.not. associated(root_test)) then
                    json_valid = .false.
                else
                    ! Check if JSON has expected structure for tokens
                    block
                        type(json_value), pointer :: tokens_test
                        logical :: found_tokens
                        call json%get('tokens', tokens_test, found_tokens)
                        if (.not. found_tokens) then
                            ! Not a valid tokens JSON file
                            json_valid = .false.
                        end if
                    end block
                end if
            end block

            if (.not. json_valid) then
                error stop "Invalid JSON format: expected tokens JSON structure"
            end if
        end block

        ! Convert JSON to tokens
        tokens = json_to_tokens(json)

        ! Clean up
        call json%destroy()

    end function json_read_tokens_from_file

    ! Convert JSON to tokens
    function json_to_tokens(json) result(tokens)
        type(json_file), intent(inout) :: json
        type(token_t), allocatable :: tokens(:)
        type(json_core) :: core
        type(json_value), pointer :: root, token_array, token_obj
        integer :: n_tokens, i
        character(len=:), allocatable :: token_type, token_text
        integer :: line, column
        logical :: found

        ! Get root object
        call json%get(root)

        ! Get tokens array
        call json%get('tokens', token_array, found)
        if (.not. found) then
            allocate (tokens(0))
            return
        end if

        ! Get number of tokens
        call core%info(token_array, n_children=n_tokens)
        allocate (tokens(n_tokens))

        ! Process each token
        do i = 1, n_tokens
            call core%get_child(token_array, i, token_obj)

            ! Get token properties
            call core%get(token_obj, 'type', token_type, found)
            call core%get(token_obj, 'text', token_text, found)
            call core%get(token_obj, 'line', line, found)
            call core%get(token_obj, 'column', column, found)

            ! Set token properties
            tokens(i)%text = token_text
            tokens(i)%line = line
            tokens(i)%column = column

            ! Convert type string to token kind
            select case (token_type)
            case ('identifier')
                tokens(i)%kind = TK_IDENTIFIER
            case ('keyword')
                tokens(i)%kind = TK_KEYWORD
            case ('operator')
                tokens(i)%kind = TK_OPERATOR
            case ('number')
                tokens(i)%kind = TK_NUMBER
            case ('string')
                tokens(i)%kind = TK_STRING
            case ('newline')
                tokens(i)%kind = TK_NEWLINE
            case ('eof')
                tokens(i)%kind = TK_EOF
            case default
                tokens(i)%kind = TK_EOF
            end select
        end do

    end function json_to_tokens

    ! Read AST from JSON file
    function json_read_ast_from_file(filename, arena) result(root_index)
        character(len=*), intent(in) :: filename
        type(ast_arena_t), intent(inout) :: arena
        integer :: root_index
        type(json_file) :: json

        ! Load JSON file
        call json%load(filename=filename)

        ! Convert JSON to AST
        root_index = json_to_ast(json, arena)

        ! Clean up
        call json%destroy()

    end function json_read_ast_from_file

    ! Convert JSON to AST (full implementation)
    function json_to_ast(json, arena) result(root_index)
        type(json_file), intent(inout) :: json
        type(ast_arena_t), intent(inout) :: arena
        integer :: root_index
        type(json_core) :: core
        type(json_value), pointer :: root
        logical :: found

        ! Get root object
        call json%get(root)

        ! Recursively build AST from JSON into arena
        root_index = json_to_ast_node(core, json_obj=root, arena=arena)

    end function json_to_ast

    ! Recursive function to convert JSON object to AST node
    recursive function json_to_ast_node(core, json_obj, arena) result(node_index)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        integer :: node_index
        character(len=:), allocatable :: node_type
        logical :: found

        ! Get node type
        call core%get(json_obj, 'type', node_type, found)
        if (.not. found) then
            ! Handle special case where root might have empty key
            block
                type(json_value), pointer :: first_child
                call core%get_child(json_obj, 1, first_child)
                if (associated(first_child)) then
                    node_index = json_to_ast_node(core, first_child, arena)
                    return
                end if
            end block
            node_index = 0  ! Invalid index
            return
        end if

        ! Create appropriate node based on type and allocate in arena
        select case (node_type)
        case ('lf_program', 'program')
            call arena%push(json_to_program_node(core, json_obj, arena))
            node_index = arena%size
        case ('assignment')
            call arena%push(json_to_assignment_node(core, json_obj, arena))
            node_index = arena%size
        case ('binary_op')
            call arena%push(json_to_binary_op_node(core, json_obj, arena))
            node_index = arena%size
        case ('identifier')
            call arena%push(json_to_identifier_node(core, json_obj, arena))
            node_index = arena%size
        case ('literal')
            call arena%push(json_to_literal_node(core, json_obj, arena))
            node_index = arena%size
        case ('function_def')
            call arena%push(json_to_function_def_node(core, json_obj, arena))
            node_index = arena%size
        case ('call_or_subscript')
            call arena%push(json_to_call_or_subscript_node(core, json_obj, arena))
            node_index = arena%size
        case ('subroutine_call')
            call arena%push(json_to_subroutine_call_node(core, json_obj, arena))
            node_index = arena%size
        case ('use_statement')
            call arena%push(json_to_use_statement_node(core, json_obj, arena))
            node_index = arena%size
        case ('include_statement')
            call arena%push(json_to_include_statement_node(core, json_obj, arena))
            node_index = arena%size
        case ('print_statement')
            call arena%push(json_to_print_statement_node(core, json_obj, arena))
            node_index = arena%size
        case default
            ! Unknown node type - create a literal as placeholder
 call arena%push(create_literal("Unknown node type: "//node_type, LITERAL_STRING, 1, 1))
            node_index = arena%size
        end select

    end function json_to_ast_node

    ! Convert JSON to program node (core program_node, not dialect-specific)
    function json_to_program_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(program_node) :: node
        character(len=:), allocatable :: name
        integer :: line, column, i
        logical :: found
        type(json_value), pointer :: body_array

        ! Get properties
        call core%get(json_obj, 'name', name, found)
        if (.not. found) name = 'main'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get body array - will be converted to indices for arena
        call core%get(json_obj, 'body', body_array, found)
        if (found) then
            node%body_indices = json_to_ast_indices(core, body_array, arena)
        end if

        ! Create core program node (dialect-agnostic)
        node%name = name
        node%line = line
        node%column = column

    end function json_to_program_node

    ! Convert JSON to assignment node
    function json_to_assignment_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node) :: node
        type(json_value), pointer :: target_obj, value_obj
        class(ast_node), allocatable :: target, value
        integer :: line, column
        logical :: found, inferred_type
        character(len=:), allocatable :: inferred_type_name

        ! Get line and column
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get type inference fields
        call core%get(json_obj, 'inferred_type', inferred_type, found)
        if (.not. found) inferred_type = .false.
        call core%get(json_obj, 'inferred_type_name', inferred_type_name, found)

        ! Get target and value indices
        call core%get(json_obj, 'target', target_obj, found)
        if (found) then
            node%target_index = json_to_ast_node(core, target_obj, arena)
        else
            call arena%push(create_identifier("unknown", line, column))
            node%target_index = arena%size
        end if

        call core%get(json_obj, 'value', value_obj, found)
        if (found) then
            node%value_index = json_to_ast_node(core, value_obj, arena)
        else
            call arena%push(create_literal("0", LITERAL_INTEGER, line, column))
            node%value_index = arena%size
        end if

        ! Set remaining node properties
        node%line = line
        node%column = column
        node%type_was_inferred = inferred_type
        if (allocated(inferred_type_name)) then
            node%inferred_type_name = inferred_type_name
        end if

    end function json_to_assignment_node

    ! Convert JSON to binary op node
    function json_to_binary_op_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node) :: node
        type(json_value), pointer :: left_obj, right_obj
        character(len=:), allocatable :: operator
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'operator', operator, found)
        if (.not. found) operator = '+'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get left and right operands and store as arena indices
        call core%get(json_obj, 'left', left_obj, found)
        if (found) then
            node%left_index = json_to_ast_node(core, left_obj, arena)
        else
            call arena%push(create_literal("0", LITERAL_INTEGER, line, column))
            node%left_index = arena%size
        end if

        call core%get(json_obj, 'right', right_obj, found)
        if (found) then
            node%right_index = json_to_ast_node(core, right_obj, arena)
        else
            call arena%push(create_literal("0", LITERAL_INTEGER, line, column))
            node%right_index = arena%size
        end if

        ! Set remaining node properties
        node%operator = operator
        node%line = line
        node%column = column

    end function json_to_binary_op_node

    ! Convert JSON to identifier node
    function json_to_identifier_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(identifier_node) :: node
        character(len=:), allocatable :: name
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'name', name, found)
        if (.not. found) name = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Create node
        node = create_identifier(name, line, column)

    end function json_to_identifier_node

    ! Convert JSON to literal node
    function json_to_literal_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(literal_node) :: node
        character(len=:), allocatable :: value, kind_str
        integer :: line, column, literal_kind
        logical :: found

        ! Get properties
        call core%get(json_obj, 'value', value, found)
        if (.not. found) value = '0'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get literal kind
        call core%get(json_obj, 'kind', kind_str, found)
        if (found) then
            select case (kind_str)
            case ('integer')
                literal_kind = LITERAL_INTEGER
            case ('real')
                literal_kind = LITERAL_REAL
            case ('string')
                literal_kind = LITERAL_STRING
            case ('logical')
                literal_kind = LITERAL_LOGICAL
            case default
                literal_kind = LITERAL_INTEGER
            end select
        else
            literal_kind = LITERAL_INTEGER
        end if

        ! Create node
        node = create_literal(value, literal_kind, line, column)

    end function json_to_literal_node

    ! Convert JSON to function definition node
    function json_to_function_def_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node) :: node
        character(len=:), allocatable :: name
        type(json_value), pointer :: body_array
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'name', name, found)
        if (.not. found) name = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get parameters
        node%param_indices = [integer::]  ! Empty parameter list for now

        ! Get return type (store as string for now)
        call core%get(json_obj, 'return_type', node%return_type, found)
        if (.not. found) node%return_type = ""

        ! Get body
        call core%get(json_obj, 'body', body_array, found)
        if (found) then
            node%body_indices = json_to_ast_indices(core, body_array, arena)
        else
            node%body_indices = [integer::]  ! Empty body
        end if

        ! Create node
        node%name = name
        node%line = line
        node%column = column

    end function json_to_function_def_node

    ! Convert JSON to call_or_subscript node
    function json_to_call_or_subscript_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node) :: node
        character(len=:), allocatable :: name
        type(json_value), pointer :: args_array
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'name', name, found)
        if (.not. found) name = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get arguments
        call core%get(json_obj, 'args', args_array, found)
        if (found) then
            node%arg_indices = json_to_ast_indices(core, args_array, arena)
        else
            node%arg_indices = [integer::]  ! Empty arguments
        end if

        ! Create node
        node%name = name
        node%line = line
        node%column = column

    end function json_to_call_or_subscript_node

    ! Convert JSON to subroutine_call node
    function json_to_subroutine_call_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_call_node) :: node
        character(len=:), allocatable :: name
        type(json_value), pointer :: args_array
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'name', name, found)
        if (.not. found) name = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get arguments
        call core%get(json_obj, 'args', args_array, found)
        if (found) then
            node%arg_indices = json_to_ast_indices(core, args_array, arena)
        else
            node%arg_indices = [integer::]  ! Empty arguments
        end if

        ! Create node
        node%name = name
        node%line = line
        node%column = column

    end function json_to_subroutine_call_node

    ! Convert JSON to use statement node
    function json_to_use_statement_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(use_statement_node) :: node
        character(len=:), allocatable :: module_name
        type(json_value), pointer :: only_array
        integer :: line, column, i, n_only
        logical :: found

        ! Get properties
        call core%get(json_obj, 'module_name', module_name, found)
        if (.not. found) module_name = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get only list
        call core%get(json_obj, 'only_list', only_array, found)
        if (found) then
            call core%info(only_array, n_children=n_only)
            if (n_only > 0) then
                allocate (node%only_list(n_only))
                do i = 1, n_only
                    block
                        type(json_value), pointer :: item
                        character(len=:), allocatable :: item_str
                        call core%get_child(only_array, i, item)
                        call core%get(item, item_str)
                        node%only_list(i)%s = item_str
                    end block
                end do
            end if
        end if

        ! Create basic node properties
        node%module_name = module_name
        node%has_only = allocated(node%only_list)
        node%line = line
        node%column = column

    end function json_to_use_statement_node

    ! Convert JSON to include statement node
    function json_to_include_statement_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(include_statement_node) :: node
        character(len=:), allocatable :: filename
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'filename', filename, found)
        if (.not. found) filename = 'unknown'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Create node
        node = create_include_statement(filename, line, column)

    end function json_to_include_statement_node

    ! Convert JSON to print statement node
    function json_to_print_statement_node(core, json_obj, arena) result(node)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_obj
        type(ast_arena_t), intent(inout) :: arena
        type(print_statement_node) :: node
        character(len=:), allocatable :: format_spec
        type(json_value), pointer :: args_array
        type(ast_node_wrapper), allocatable :: args(:)
        integer :: line, column
        logical :: found

        ! Get properties
        call core%get(json_obj, 'format_spec', format_spec, found)
        if (.not. found) format_spec = '*'
        call core%get(json_obj, 'line', line, found)
        if (.not. found) line = 1
        call core%get(json_obj, 'column', column, found)
        if (.not. found) column = 1

        ! Get arguments
        call core%get(json_obj, 'args', args_array, found)
        if (found) then
            node%arg_indices = json_to_ast_indices(core, args_array, arena)
        else
            node%arg_indices = [integer::]  ! Empty arguments
        end if

        ! Create node
        node%format_spec = format_spec
        node%line = line
        node%column = column

    end function json_to_print_statement_node

    ! Convert JSON array to arena indices array
    function json_to_ast_indices(core, json_array, arena) result(indices)
        type(json_core), intent(inout) :: core
        type(json_value), pointer, intent(in) :: json_array
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: indices(:)
        integer :: i, n_elements
        type(json_value), pointer :: element

        ! Get array size
        call core%info(json_array, n_children=n_elements)
        allocate (indices(n_elements))

        ! Process each element
        do i = 1, n_elements
            call core%get_child(json_array, i, element)
            indices(i) = json_to_ast_node(core, element, arena)
        end do

    end function json_to_ast_indices

    ! Read semantic analysis result from JSON file
    subroutine json_read_semantic_from_file(filename, arena, root_index, sem_ctx)
        character(len=*), intent(in) :: filename
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: root_index
        type(semantic_context_t), intent(out) :: sem_ctx
        type(json_file) :: json

        ! Load JSON file
        call json%load(filename=filename)

        ! Convert JSON to semantic analysis result
        call json_to_semantic(json, arena, root_index, sem_ctx)

        ! Clean up
        call json%destroy()

    end subroutine json_read_semantic_from_file

    ! Convert JSON to semantic analysis result (AST + context)
    subroutine json_to_semantic(json, arena, root_index, sem_ctx)
        type(json_file), intent(inout) :: json
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: root_index
        type(semantic_context_t), intent(out) :: sem_ctx
        type(json_core) :: core
        type(json_value), pointer :: root, ast_obj
        logical :: found

        ! Get root object
        call json%get(root)

        ! Initialize semantic context
        sem_ctx = create_semantic_context()

        ! Get annotated AST from JSON
        call core%get(root, 'annotated_ast', ast_obj, found)
        if (found) then
            root_index = json_to_ast_node(core, ast_obj, arena)
        else
            ! Fallback: try to get regular AST and reconstruct type info
            root_index = json_to_ast_node(core, root, arena)
        end if

        ! Type environment reconstruction is handled by the type inference metadata
        ! already stored in the AST nodes (inferred_type, inferred_type_name)
        ! The semantic context will be rebuilt when needed during further processing

    end subroutine json_to_semantic

end module json_reader
