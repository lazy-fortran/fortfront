module parser_core
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression, parse_logical_or, &
          parse_logical_and, parse_comparison, parse_concatenation, parse_term, &
          parse_factor, parse_power, parse_unary, parse_primary
    use parser_definition_statements_module, only: parse_function_definition, &
                                               parse_subroutine_definition, &
                                               parse_interface_block
    use parser_import_statements_module, only: parse_module
    use parser_control_flow_module, only: parse_do_loop, parse_do_while, &
                                          parse_select_case, parse_if, &
                                          parse_if_condition, parse_if_body
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use ast_core, only: ast_node, ast_node_wrapper, ast_arena_t, create_ast_arena, &
             assignment_node, binary_op_node, identifier_node, literal_node, &
             call_or_subscript_node, function_def_node, subroutine_def_node, &
             print_statement_node, use_statement_node, include_statement_node, &
             declaration_node, do_loop_node, do_while_node, if_node, elseif_wrapper, &
             select_case_node, case_wrapper, derived_type_node, interface_block_node, &
             module_node, create_assignment, create_binary_op, create_identifier, &
             create_literal, create_call_or_subscript, create_function_def, &
             create_subroutine_def, create_print_statement, create_use_statement, &
             create_include_statement, create_declaration, &
                       create_do_loop, create_do_while, create_if, create_select_case, &
                        create_derived_type, create_interface_block, create_module, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    implicit none
    private

    ! Public parsing interface
    public :: parse_expression
    public :: parse_primary
    public :: parse_function_definition, parse_subroutine_definition

contains

    ! REMOVED: parse_statement - use the arena-based version in &
    ! parser_dispatcher_module instead

    ! REMOVED: parse_declaration - use the arena-based version in &
    ! parser_declarations_module instead

    ! Parse array dimensions inside parentheses
    subroutine parse_array_dimensions(parser, dimensions)
        type(parser_state_t), intent(inout) :: parser
        type(ast_node_wrapper), allocatable, intent(out) :: dimensions(:)

        type(token_t) :: token
        type(ast_node_wrapper), allocatable :: temp_dims(:)
        integer :: dim_count

        ! Initialize
        dim_count = 0
        allocate (temp_dims(10))  ! Initial allocation

        ! Parse dimensions separated by commas
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                ! End of dimensions
                exit
            end if

            dim_count = dim_count + 1
            if (dim_count > size(temp_dims)) then
                ! Need to resize
                block
                    type(ast_node_wrapper), allocatable :: new_dims(:)
                    allocate (new_dims(size(temp_dims)*2))
                    new_dims(1:size(temp_dims)) = temp_dims
                    ! Use move_alloc for O(1) array transfer instead of O(n) copying
                    call move_alloc(new_dims, temp_dims)
                end block
            end if

            ! Parse a single dimension
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                ! Assumed shape dimension (:)
                token = parser%consume()
        allocate (temp_dims(dim_count)%node, source=create_literal(":", LITERAL_STRING))
            else if (token%kind == TK_NUMBER) then
                ! Could be fixed size dimension (10) or bounds notation (1:10)
                block
                    character(len=:), allocatable :: first_number
                    type(token_t) :: next_token

                    first_number = token%text
                    token = parser%consume()

                    ! Check if followed by colon for bounds notation
                    next_token = parser%peek()
                   if (next_token%kind == TK_OPERATOR .and. next_token%text == ":") then
                        ! Bounds notation like 1:10
                        token = parser%consume()  ! consume ':'
                        next_token = parser%peek()
                        if (next_token%kind == TK_NUMBER) then
                            ! Complete bounds notation
                            token = parser%consume()
                            allocate (temp_dims(dim_count)%node, &
                   source=create_literal(first_number//":"//token%text, LITERAL_STRING))
                        else
                            ! Just first_number:
                            allocate (temp_dims(dim_count)%node, &
                                source=create_literal(first_number//":", LITERAL_STRING))
                        end if
                    else
                        ! Just a fixed size dimension
                        allocate (temp_dims(dim_count)%node, &
                            source=create_literal(first_number, LITERAL_INTEGER))
                    end if
                end block
            else
                ! More complex dimension expressions - for now, just consume &
                ! as identifier
                token = parser%consume()
 allocate (temp_dims(dim_count)%node, source=create_literal(token%text, LITERAL_STRING))
            end if

            ! Check for comma (multiple dimensions)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! Consume comma
                cycle
            else
                exit
            end if
        end do

        ! Copy to result
        if (dim_count > 0) then
            allocate (dimensions(dim_count))
            dimensions(1:dim_count) = temp_dims(1:dim_count)
        end if

    end subroutine parse_array_dimensions

    ! REMOVED: parse_derived_type - use the arena-based version in &
    ! parser_declarations_module instead
    ! REMOVED: parse_derived_type_parameters - use the arena-based version &
    ! in parser_declarations_module instead

    ! REMOVED: parse_print_statement - use the arena-based version in &
    ! parser_statements_module instead

    ! REMOVED: parse_use_statement - use the arena-based version in &
    ! parser_statements_module instead
    ! REMOVED: parse_include_statement - use the arena-based version in &
    ! parser_statements_module instead

    ! Parse do loop: do var = start, end [, step]

    ! Parse interface block: interface [name|operator(op)] ... end interface

    ! Parse only list: item1, item2, new_name => old_name
    subroutine parse_only_list(parser, only_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: only_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)

        character(len=:), allocatable :: temp_only(:)
        character(len=:), allocatable :: temp_rename(:)
        type(token_t) :: token
        integer :: count
        character(len=:), allocatable :: item_name, old_name

        ! Allocate temporary arrays to avoid stack overflow
        allocate(character(len=100) :: temp_only(50))
        allocate(character(len=100) :: temp_rename(50))
        
        count = 0
        temp_only = ""
        temp_rename = ""

        ! Parse first item
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            item_name = token%text

            ! Check if this is a rename (new_name => old_name)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ">") then
                    token = parser%consume()  ! consume '>'

                    ! Get old name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        old_name = token%text

                        ! Store rename: "new_name => old_name"
                        count = count + 1
                        temp_rename(count) = item_name//" => "//old_name
                    end if
                end if
            else
                ! Regular only item
                count = count + 1
                temp_only(count) = item_name
            end if
        end if

        ! Parse remaining items
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','

                ! Get next item
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                    item_name = token%text

                    ! Check if this is a rename
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        token = parser%consume()  ! consume '='
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ">") then
                            token = parser%consume()  ! consume '>'

                            ! Get old name
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                                old_name = token%text

                                ! Store rename: "new_name => old_name"
                                count = count + 1
                                temp_rename(count) = item_name//" => "//old_name
                            end if
                        end if
                    else
                        ! Regular only item
                        count = count + 1
                        temp_only(count) = item_name
                    end if
                else
                    ! No more items
                    exit
                end if
            else
                ! No more items
                exit
            end if
        end do

        ! Copy to output arrays with proper size
        block
            integer :: only_count, rename_count, i

            ! Count actual items
            only_count = 0
            rename_count = 0
            do i = 1, count
                if (len_trim(temp_only(i)) > 0) only_count = only_count + 1
                if (len_trim(temp_rename(i)) > 0) rename_count = rename_count + 1
            end do

            ! Allocate output arrays
            if (only_count > 0) then
                allocate (character(len=100) :: only_list(only_count))
                only_count = 0
                do i = 1, count
                    if (len_trim(temp_only(i)) > 0) then
                        only_count = only_count + 1
                        only_list(only_count) = trim(temp_only(i))
                    end if
                end do
            end if

            if (rename_count > 0) then
                allocate (character(len=100) :: rename_list(rename_count))
                rename_count = 0
                do i = 1, count
                    if (len_trim(temp_rename(i)) > 0) then
                        rename_count = rename_count + 1
                        rename_list(rename_count) = trim(temp_rename(i))
                    end if
                end do
            end if
        end block

    end subroutine parse_only_list

    ! Parse if statement

    ! Parse elseif block
    function parse_elseif_block(parser) result(elseif_block)
        type(parser_state_t), intent(inout) :: parser
        type(elseif_wrapper) :: elseif_block
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()

        ! Parse condition expression - simplified for now
        ! For now, store as 0 until full arena-based elseif parsing is implemented
        elseif_block%condition_index = 0

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body statements until else/elseif/endif
        block
            integer, allocatable :: temp_body_indices(:)
            allocate (temp_body_indices(0))  ! Start with empty array

            ! For now, use empty body until full arena-based parsing is implemented
            elseif_block%body_indices = temp_body_indices
        end block

    end function parse_elseif_block

end module parser_core
