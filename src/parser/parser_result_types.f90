module parser_result_types
    use error_handling, only: result_t, success_result, create_error_result, &
                             ERROR_ERROR, ERROR_PARSER, error_collection_t, &
                             create_error_collection, ERROR_WARNING
    implicit none
    private

    ! Parser result type for single AST nodes
    type, public :: parse_result_t
        type(result_t) :: result
        integer :: node_index = 0  ! AST node index if successful
    contains
        procedure :: is_success => parse_result_is_success
        procedure :: is_failure => parse_result_is_failure
        procedure :: get_node => parse_result_get_node
        procedure :: set_error => parse_result_set_error
        procedure :: set_success => parse_result_set_success
    end type parse_result_t

    ! Compilation result for top-level parsing
    type, public :: compile_result_t
        type(result_t) :: result
        integer :: program_index = 0  ! Root AST node if successful
        type(error_collection_t) :: warnings
    contains
        procedure :: is_success => compile_result_is_success
        procedure :: is_failure => compile_result_is_failure
        procedure :: get_program => compile_result_get_program
        procedure :: has_warnings => compile_result_has_warnings
        procedure :: get_warning_count => compile_result_get_warning_count
        procedure :: add_warning => compile_result_add_warning
        procedure :: set_error => compile_result_set_error
        procedure :: set_success => compile_result_set_success
    end type compile_result_t

    ! Factory functions
    public :: success_parse_result, error_parse_result
    public :: success_compile_result, error_compile_result

    ! Error recovery functions
    public :: recover_to_statement_boundary, recover_to_closing_paren
    public :: recover_to_next_token, skip_to_synchronization_point

contains

    ! parse_result_t methods
    pure function parse_result_is_success(this) result(success)
        class(parse_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success()
    end function parse_result_is_success

    pure function parse_result_is_failure(this) result(failure)
        class(parse_result_t), intent(in) :: this
        logical :: failure
        failure = this%result%is_failure()
    end function parse_result_is_failure

    pure function parse_result_get_node(this) result(node_index)
        class(parse_result_t), intent(in) :: this
        integer :: node_index
        node_index = this%node_index
    end function parse_result_get_node

    subroutine parse_result_set_error(this, message, code, component, context, suggestion)
        class(parse_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        call this%result%set_error(message, code, component, context, suggestion)
        this%node_index = 0
    end subroutine parse_result_set_error

    subroutine parse_result_set_success(this, node_index)
        class(parse_result_t), intent(inout) :: this
        integer, intent(in) :: node_index
        
        this%result = success_result()
        this%node_index = node_index
    end subroutine parse_result_set_success

    ! compile_result_t methods
    pure function compile_result_is_success(this) result(success)
        class(compile_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success()
    end function compile_result_is_success

    pure function compile_result_is_failure(this) result(failure)
        class(compile_result_t), intent(in) :: this
        logical :: failure
        failure = this%result%is_failure()
    end function compile_result_is_failure

    pure function compile_result_get_program(this) result(program_index)
        class(compile_result_t), intent(in) :: this
        integer :: program_index
        program_index = this%program_index
    end function compile_result_get_program

    function compile_result_has_warnings(this) result(has_warn)
        class(compile_result_t), intent(in) :: this
        logical :: has_warn
        has_warn = this%warnings%get_error_count() > 0
    end function compile_result_has_warnings

    function compile_result_get_warning_count(this) result(count)
        class(compile_result_t), intent(in) :: this
        integer :: count
        count = this%warnings%get_error_count()
    end function compile_result_get_warning_count

    subroutine compile_result_add_warning(this, message, code, component, context, suggestion)
        class(compile_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        ! Initialize warnings collection if not already done
        if (this%warnings%get_error_count() == 0 .and. .not. allocated(this%warnings%errors)) then
            this%warnings = create_error_collection()
        end if
        
        call this%warnings%add_error(message, code, severity=ERROR_WARNING, &
                                     component=component, context=context, &
                                     suggestion=suggestion)
    end subroutine compile_result_add_warning

    subroutine compile_result_set_error(this, message, code, component, context, suggestion)
        class(compile_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        call this%result%set_error(message, code, component, context, suggestion)
        this%program_index = 0
    end subroutine compile_result_set_error

    subroutine compile_result_set_success(this, program_index)
        class(compile_result_t), intent(inout) :: this
        integer, intent(in) :: program_index
        
        this%result = success_result()
        this%program_index = program_index
    end subroutine compile_result_set_success

    ! Factory functions
    function success_parse_result(node_index) result(parse_res)
        integer, intent(in) :: node_index
        type(parse_result_t) :: parse_res
        
        call parse_res%set_success(node_index)
    end function success_parse_result

    function error_parse_result(message, code, component, context, suggestion) result(parse_res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(parse_result_t) :: parse_res
        
        call parse_res%set_error(message, code, component, context, suggestion)
    end function error_parse_result

    function success_compile_result(program_index) result(compile_res)
        integer, intent(in) :: program_index
        type(compile_result_t) :: compile_res
        
        ! Initialize warnings collection
        compile_res%warnings = create_error_collection()
        call compile_res%set_success(program_index)
    end function success_compile_result

    function error_compile_result(message, code, component, context, suggestion) result(compile_res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(compile_result_t) :: compile_res
        
        ! Initialize warnings collection
        compile_res%warnings = create_error_collection()
        call compile_res%set_error(message, code, component, context, suggestion)
    end function error_compile_result

    ! Error recovery strategies for continued parsing after errors
    
    ! Recover to statement boundary (newline, semicolon, or end keyword)
    function recover_to_statement_boundary(parser) result(recovery_res)
        use parser_state_module, only: parser_state_t
        use lexer_core, only: TK_KEYWORD, TK_NEWLINE, TK_OPERATOR, TK_EOF
        type(parser_state_t), intent(inout) :: parser
        type(result_t) :: recovery_res
        
        integer :: tokens_skipped
        
        tokens_skipped = 0
        
        ! Skip tokens until we find a statement boundary
        do while (.not. parser%is_at_end())
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                
                token = parser%peek()
                
                ! Statement boundary tokens
                if (token%kind == TK_NEWLINE) then
                    exit  ! Found newline - good statement boundary
                end if
                
                if (token%kind == TK_OPERATOR .and. token%text == ";") then
                    exit  ! Found semicolon - statement separator
                end if
                
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("end", "contains", "else", "elseif", "endif", &
                          "enddo", "endfunction", "endsubroutine", &
                          "endmodule", "endprogram", "endinterface")
                        exit  ! Found end keyword - statement boundary
                    case ("integer", "real", "logical", "character", &
                          "complex", "type", "function", "subroutine", &
                          "program", "module", "if", "do", "select", &
                          "use", "include", "call", "print", "write", &
                          "read", "allocate", "deallocate")
                        exit  ! Found statement-starting keyword
                    end select
                end if
                
                token = parser%consume()
                tokens_skipped = tokens_skipped + 1
                
                ! Safety check - don't skip too many tokens
                if (tokens_skipped > 100) then
                    recovery_res = create_error_result( &
                        "Error recovery failed: too many tokens skipped", &
                        ERROR_PARSER, &
                        "parser_result_types", &
                        "recover_to_statement_boundary", &
                        "Check for missing statement terminators" &
                    )
                    return
                end if
            end block
        end do
        
        recovery_res = success_result()
    end function recover_to_statement_boundary

    ! Recover to closing parenthesis, bracket, or brace
    function recover_to_closing_paren(parser, opening_char) result(recovery_res)
        use parser_state_module, only: parser_state_t
        use lexer_core, only: TK_OPERATOR, TK_EOF
        type(parser_state_t), intent(inout) :: parser
        character(len=1), intent(in) :: opening_char
        type(result_t) :: recovery_res
        
        character(len=1) :: closing_char
        integer :: nesting_level, tokens_skipped
        
        ! Determine closing character
        select case (opening_char)
        case ("(")
            closing_char = ")"
        case ("[")
            closing_char = "]"
        case ("{")
            closing_char = "}"
        case default
            recovery_res = create_error_result( &
                "Invalid opening character for recovery", &
                ERROR_PARSER, &
                "parser_result_types", &
                "recover_to_closing_paren", &
                "Use '(', '[', or '{' as opening character" &
            )
            return
        end select
        
        nesting_level = 1
        tokens_skipped = 0
        
        do while (.not. parser%is_at_end() .and. nesting_level > 0)
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                
                token = parser%peek()
                
                if (token%kind == TK_OPERATOR) then
                    if (token%text == opening_char) then
                        nesting_level = nesting_level + 1
                    else if (token%text == closing_char) then
                        nesting_level = nesting_level - 1
                    end if
                end if
                
                token = parser%consume()
                tokens_skipped = tokens_skipped + 1
                
                ! Safety check
                if (tokens_skipped > 200) then
                    recovery_res = create_error_result( &
                        "Error recovery failed: unmatched " // opening_char, &
                        ERROR_PARSER, &
                        "parser_result_types", &
                        "recover_to_closing_paren", &
                        "Check for missing " // closing_char &
                    )
                    return
                end if
            end block
        end do
        
        if (nesting_level > 0) then
            recovery_res = create_error_result( &
                "Reached end of input with unmatched " // opening_char, &
                ERROR_PARSER, &
                "parser_result_types", &
                "recover_to_closing_paren", &
                "Add missing " // closing_char &
            )
        else
            recovery_res = success_result()
        end if
    end function recover_to_closing_paren

    ! Recover to next occurrence of specific token
    function recover_to_next_token(parser, target_kind, target_text) result(recovery_res)
        use parser_state_module, only: parser_state_t
        use lexer_core, only: TK_EOF
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: target_kind
        character(len=*), intent(in), optional :: target_text
        type(result_t) :: recovery_res
        
        integer :: tokens_skipped
        
        tokens_skipped = 0
        
        do while (.not. parser%is_at_end())
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                
                token = parser%peek()
                
                if (token%kind == target_kind) then
                    if (present(target_text)) then
                        if (token%text == target_text) then
                            exit  ! Found target token with matching text
                        end if
                    else
                        exit  ! Found target token kind
                    end if
                end if
                
                token = parser%consume()
                tokens_skipped = tokens_skipped + 1
                
                ! Safety check
                if (tokens_skipped > 150) then
                    recovery_res = create_error_result( &
                        "Error recovery failed: target token not found", &
                        ERROR_PARSER, &
                        "parser_result_types", &
                        "recover_to_next_token", &
                        "Check for missing or malformed tokens" &
                    )
                    return
                end if
            end block
        end do
        
        recovery_res = success_result()
    end function recover_to_next_token

    ! Skip to known synchronization points for robust error recovery
    function skip_to_synchronization_point(parser) result(recovery_res)
        use parser_state_module, only: parser_state_t
        use lexer_core, only: TK_KEYWORD, TK_EOF
        type(parser_state_t), intent(inout) :: parser
        type(result_t) :: recovery_res
        
        integer :: tokens_skipped
        
        tokens_skipped = 0
        
        do while (.not. parser%is_at_end())
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                
                token = parser%peek()
                
                ! Synchronization points - major structural keywords
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("program", "module", "function", "subroutine", &
                          "interface", "type", "contains", &
                          "end", "endprogram", "endmodule", "endfunction", &
                          "endsubroutine", "endinterface", "endtype")
                        exit  ! Found major structural boundary
                    case ("use", "include", "implicit")
                        exit  ! Found declaration boundary
                    end select
                end if
                
                token = parser%consume()
                tokens_skipped = tokens_skipped + 1
                
                ! Safety check
                if (tokens_skipped > 300) then
                    recovery_res = create_error_result( &
                        "Error recovery failed: no synchronization point found", &
                        ERROR_PARSER, &
                        "parser_result_types", &
                        "skip_to_synchronization_point", &
                        "Check for missing end statements or malformed structure" &
                    )
                    return
                end if
            end block
        end do
        
        recovery_res = success_result()
    end function skip_to_synchronization_point

end module parser_result_types