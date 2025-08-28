module parser_utils
    use lexer_core, only: token_t, TK_OPERATOR, TK_KEYWORD, TK_IDENTIFIER, TK_EOF
    use parser_state_module, only: parser_state_t
    implicit none
    private
    
    public :: analyze_declaration_structure

contains

    ! Analyze declaration structure to determine if single or multi-variable
    subroutine analyze_declaration_structure(parser, has_initializer, has_comma)
        use iso_fortran_env, only: error_unit
        type(parser_state_t), intent(inout) :: parser
        logical, intent(out) :: has_initializer, has_comma
        integer :: lookahead_pos
        type(token_t) :: lookahead_token
        type(token_t), allocatable :: tokens(:)
        
        has_initializer = .false.
        has_comma = .false.
        lookahead_pos = parser%current_token
        
        ! Get tokens directly from parser
        if (allocated(parser%tokens)) then
            allocate(tokens(size(parser%tokens)))
            tokens = parser%tokens
        else
            allocate(tokens(0))
        end if
        
        ! Defensive check for empty token array
        if (size(tokens) == 0) then
            return  ! Safe defaults: no initializer, no comma
        end if
        
        call scan_tokens(tokens, lookahead_pos, has_initializer, has_comma)
    end subroutine analyze_declaration_structure

    ! Core token scanning logic (kept under 50 lines per CLAUDE.md)
    subroutine scan_tokens(tokens, lookahead_pos, has_initializer, has_comma)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: lookahead_pos
        logical, intent(inout) :: has_initializer, has_comma
        
        logical :: seen_double_colon, in_brackets, in_attributes, should_exit
        integer :: bracket_depth, variable_count_after_colon, start_pos, relative_pos
        type(token_t) :: lookahead_token
        
        seen_double_colon = .false.
        in_brackets = .false.
        in_attributes = .true.  ! Start assuming we're in the attribute section
        bracket_depth = 0
        variable_count_after_colon = 0
        start_pos = lookahead_pos  ! Remember starting position
        
        do while (lookahead_pos <= size(tokens))
            if (lookahead_pos > size(tokens)) exit
            lookahead_token = tokens(lookahead_pos)
            relative_pos = lookahead_pos - start_pos + 1  ! Calculate relative position
            
            call process_token(lookahead_token, relative_pos, &
                seen_double_colon, in_brackets, in_attributes, &
                bracket_depth, variable_count_after_colon, &
                has_initializer, has_comma, should_exit)
                
            if (has_comma .or. should_exit) exit
            lookahead_pos = lookahead_pos + 1
        end do
        
        ! Final check: single variable (with or without initializer)
        ! Only override if no comma was found during scan
        if (variable_count_after_colon == 1 .and. .not. has_comma) then
            has_comma = .false.
        end if
    end subroutine scan_tokens

    ! Process individual token (kept under 50 lines per CLAUDE.md)
    subroutine process_token(token, pos, seen_colon, in_brackets, in_attr, &
                           bracket_depth, var_count, has_init, has_comma, should_exit)
        type(token_t), intent(in) :: token
        integer, intent(in) :: pos
        logical, intent(inout) :: seen_colon, in_brackets, in_attr
        integer, intent(inout) :: bracket_depth, var_count
        logical, intent(inout) :: has_init, has_comma
        logical, intent(out) :: should_exit
        
        should_exit = .false.
        
        if (token%kind == TK_OPERATOR) then
            call process_operator(token, seen_colon, in_brackets, in_attr, &
                                bracket_depth, var_count, has_init, has_comma)
        else if (token%kind == TK_IDENTIFIER .and. seen_colon .and. .not. in_brackets) then
            var_count = var_count + 1
        else if (token%kind == TK_KEYWORD) then
            if (pos > 1 .and. .not. is_attribute_keyword(token%text)) then
                should_exit = .true.
            end if
        else if (token%kind == TK_EOF) then
            should_exit = .true.
        end if
    end subroutine process_token

    ! Process operator tokens
    subroutine process_operator(token, seen_colon, in_brackets, in_attr, &
                              bracket_depth, var_count, has_init, has_comma)
        type(token_t), intent(in) :: token
        logical, intent(inout) :: seen_colon, in_brackets, in_attr
        integer, intent(inout) :: bracket_depth, var_count
        logical, intent(inout) :: has_init, has_comma
        
        if (token%text == "(" .or. token%text == "[") then
            bracket_depth = bracket_depth + 1
            in_brackets = (bracket_depth > 0)
        else if (token%text == ")" .or. token%text == "]") then
            bracket_depth = bracket_depth - 1
            in_brackets = (bracket_depth > 0)
        else if (token%text == "::") then
            seen_colon = .true.
            in_attr = .false.
        else if (token%text == "=") then
            has_init = .true.
        else if (token%text == ",") then
            if (seen_colon .and. .not. in_brackets .and. .not. in_attr) then
                has_comma = .true.
            end if
        end if
    end subroutine process_operator

    ! Check if keyword is an attribute (not statement-ending)
    logical function is_attribute_keyword(keyword)
        character(len=*), intent(in) :: keyword
        is_attribute_keyword = (keyword == "parameter" .or. &
                               keyword == "intent" .or. &
                               keyword == "optional" .or. &
                               keyword == "allocatable" .or. &
                               keyword == "pointer" .or. &
                               keyword == "target" .or. &
                               keyword == "save" .or. &
                               keyword == "public" .or. &
                               keyword == "private")
    end function is_attribute_keyword

end module parser_utils