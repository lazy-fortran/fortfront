module parser_type_specifications_module
    ! Type specification parsing module for implicit statements
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory, only: push_implicit_statement
    implicit none
    private

    public :: parse_implicit_statement

contains

    ! Helper to count letter ranges for allocation
    subroutine count_letter_ranges(parser, count)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: count
        type(token_t) :: token
        integer :: saved_pos
        
        count = 0
        saved_pos = parser%current_token
        
        do
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                count = count + 1
                token = parser%consume()
                
                ! Check for range (a-h)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
        
        ! Restore parser position
        parser%current_token = saved_pos
    end subroutine count_letter_ranges

    ! Helper to parse letter ranges into array
    subroutine parse_letter_ranges(parser, letter_ranges)
        type(parser_state_t), intent(inout) :: parser
        character(len=64), intent(out) :: letter_ranges(:)
        type(token_t) :: token
        integer :: i
        
        i = 1
        do while (i <= size(letter_ranges))
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                letter_ranges(i) = token%text
                token = parser%consume()
                
                ! Check for range (a-h)  
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        letter_ranges(i) = trim(letter_ranges(i)) // "-" // token%text
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                i = i + 1
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then  
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end subroutine parse_letter_ranges

    ! Parse character type specification (extracted from parse_implicit_statement)
    subroutine parse_character_type_spec(parser, length_value, has_length)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: length_value
        logical, intent(out) :: has_length
        type(token_t) :: token

        has_length = .false.
        length_value = 0

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "len") then
            token = parser%consume()  ! consume 'len'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
            end if
        end if
        if (token%kind == TK_NUMBER) then
            read(token%text, *) length_value
            has_length = .true.
            token = parser%consume()
        end if
    end subroutine parse_character_type_spec

    ! Parse kind specification (extracted from parse_implicit_statement)
    subroutine parse_kind_specification(parser, kind_value, has_kind)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        type(token_t) :: token

        has_kind = .false.
        kind_value = 0

        token = parser%peek()
        if (token%kind == TK_NUMBER) then
            read(token%text, *) kind_value
            has_kind = .true.
            token = parser%consume()
        end if
    end subroutine parse_kind_specification

    ! Check if parentheses contain type specification (extracted logic)
    function is_type_specification(parser, type_name) result(is_type_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: type_name
        logical :: is_type_spec
        type(token_t) :: token
        integer :: saved_pos

        is_type_spec = .false.
        saved_pos = parser%current_token

        token = parser%consume()  ! consume '('
        token = parser%peek()

        if (type_name == "character") then
            ! For character: check for "len" keyword or number
            if ((token%kind == TK_KEYWORD .and. token%text == "len") .or. &
                token%kind == TK_NUMBER) then
                is_type_spec = .true.
            end if
        else
            ! For other types: check for number (kind specification)
            if (token%kind == TK_NUMBER) then
                is_type_spec = .true.
            end if
        end if

        ! Restore position
        parser%current_token = saved_pos
    end function is_type_specification

    function parse_implicit_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: kind_value, length_value
        logical :: has_kind, has_length, is_none
        character(len=64), allocatable :: letter_ranges(:)
        integer :: line, column, range_count
        
        ! Consume 'implicit' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Check for 'none'
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "none") then
            token = parser%consume()  ! consume 'none'
            is_none = .true.
            stmt_index = push_implicit_statement(arena, is_none, &
                                                line=line, column=column, parent_index=0)
            return
        end if
        
        ! Parse type specification
        is_none = .false.
        has_kind = .false.
        has_length = .false.
        kind_value = 0
        length_value = 0
        range_count = 0
        
        ! Get type name
        if (token%kind == TK_KEYWORD) then
            type_name = token%text
            token = parser%consume()
            
            ! Check for kind or length specification
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                if (is_type_specification(parser, type_name)) then
                    ! Parse as type specification
                    token = parser%consume()  ! consume '('
                    
                    if (type_name == "character") then
                        call parse_character_type_spec(parser, length_value, has_length)
                    else
                        call parse_kind_specification(parser, kind_value, has_kind)
                    end if
                    
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()  ! consume ')'
                    end if
                end if
            end if
        end if
        
        ! Parse letter specification list in parentheses
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Count letter ranges first
            call count_letter_ranges(parser, range_count)
            
            if (range_count > 0) then
                allocate(letter_ranges(range_count))
                call parse_letter_ranges(parser, letter_ranges)
            end if
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        ! Create implicit statement node
        stmt_index = push_implicit_statement(arena, is_none, type_name, kind_value, &
                                           has_kind, length_value, has_length, &
                                           letter_ranges, line, column, parent_index=0)
    end function parse_implicit_statement

end module parser_type_specifications_module