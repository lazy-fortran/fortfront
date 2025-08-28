module parser_utilities
    ! Utility parsing functions for common parsing operations
    ! Contains helper functions for identifier lists, letter ranges, and type specs
    
    use lexer_core
    use parser_state_module
    use ast_core
    
    implicit none
    private
    
    ! Public utility functions
    public :: parse_identifier_list, parse_letter_ranges, parse_character_type_spec, &
              parse_kind_specification, is_type_specification, skip_to_end_of_line
    
contains

    ! Parse comma-separated list of identifiers (for only clause)
    subroutine parse_identifier_list(parser, identifier_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: identifier_list(:)
        character(len=:), allocatable :: temp_list(:)
        type(token_t) :: token
        integer :: count, capacity
        
        count = 0
        capacity = 4  ! Initial capacity
        allocate(character(len=64) :: temp_list(capacity))
        
        ! Parse first identifier
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            count = count + 1
            temp_list(count) = token%text
            
            ! Parse additional identifiers (comma-separated)
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        
                        ! Expand array if needed
                        if (count >= capacity) then
                            capacity = capacity * 2
                            block
                                character(len=64), allocatable :: new_temp_list(:)
                                integer :: i
                                allocate(new_temp_list(capacity))
                                do i = 1, count
                                    new_temp_list(i) = temp_list(i)
                                end do
                                deallocate(temp_list)
                                call move_alloc(new_temp_list, temp_list)
                            end block
                        end if
                        
                        count = count + 1
                        temp_list(count) = token%text
                    else
                        exit  ! Not an identifier after comma
                    end if
                else
                    exit  ! Not a comma, end of list
                end if
            end do
        end if
        
        ! Copy to final array with exact size
        if (count > 0) then
            allocate(character(len=64) :: identifier_list(count))
            identifier_list(1:count) = temp_list(1:count)
        else
            allocate(character(len=0) :: identifier_list(0))
        end if
        
        deallocate(temp_list)
    end subroutine parse_identifier_list

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
            if (token%kind == TK_IDENTIFIER .and. len_trim(token%text) == 1) then
                count = count + 1
                token = parser%consume()
                
                ! Check for range (a-z)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len_trim(token%text) == 1) then
                        token = parser%consume()  ! consume second letter
                    end if
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit  ! No comma, end of list
                end if
            else
                exit  ! Not a single letter
            end if
        end do
        
        parser%current_token = saved_pos
    end subroutine count_letter_ranges

    ! Parse letter ranges for implicit statements (e.g., a-c, i-n)
    subroutine parse_letter_ranges(parser, letter_ranges)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: letter_ranges(:)
        integer :: count, i
        type(token_t) :: token
        
        ! Count ranges first
        call count_letter_ranges(parser, count)
        
        if (count == 0) then
            allocate(character(len=0) :: letter_ranges(0))
            return
        end if
        
        allocate(character(len=8) :: letter_ranges(count))
        
        i = 0
        do
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len_trim(token%text) == 1) then
                i = i + 1
                letter_ranges(i) = trim(token%text)
                token = parser%consume()
                
                ! Check for range (a-z)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len_trim(token%text) == 1) then
                        token = parser%consume()  ! consume second letter
                        letter_ranges(i) = trim(letter_ranges(i)) // "-" // trim(token%text)
                    end if
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit  ! No comma, end of list
                end if
            else
                exit  ! Not a single letter
            end if
        end do
    end subroutine parse_letter_ranges

    ! Parse character type specification like character(len=20)
    subroutine parse_character_type_spec(parser, length_value, has_length)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: length_value
        logical, intent(out) :: has_length
        type(token_t) :: token
        
        has_length = .false.
        token = parser%peek()
        
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            token = parser%peek()
            
            if (token%kind == TK_IDENTIFIER .and. trim(token%text) == "len") then
                token = parser%consume()  ! consume 'len'
                token = parser%peek()
                
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume '='
                    token = parser%peek()
                    
                    if (token%kind == TK_LITERAL) then
                        token = parser%consume()
                        length_value = token%text
                        has_length = .true.
                    end if
                end if
            end if
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        if (.not. has_length) then
            length_value = "1"  ! Default character length
        end if
    end subroutine parse_character_type_spec

    ! Parse kind specification like integer(kind=int32)
    subroutine parse_kind_specification(parser, kind_value, has_kind)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        type(token_t) :: token
        
        has_kind = .false.
        kind_value = ""
        
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            token = parser%peek()
            
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_LITERAL) then
                token = parser%consume()
                kind_value = token%text
                has_kind = .true.
            end if
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
    end subroutine parse_kind_specification

    ! Check if current token sequence represents a type specification
    function is_type_specification(parser, type_name) result(is_type_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: type_name
        logical :: is_type_spec
        type(token_t) :: token
        
        is_type_spec = .false.
        token = parser%peek()
        
        if (token%kind == TK_IDENTIFIER) then
            select case (trim(token%text))
            case ("integer", "real", "logical", "character", "complex", &
                  "double", "byte")
                is_type_spec = .true.
                type_name = token%text
                
                ! Handle "double precision"
                if (trim(token%text) == "double") then
                    token = parser%consume()  ! consume 'double'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. trim(token%text) == "precision") then
                        type_name = "double precision"
                    else
                        ! Put back the token
                        parser%current_token = parser%current_token - 1
                        type_name = "double"
                    end if
                end if
                
            case default
                type_name = ""
            end select
        else
            type_name = ""
        end if
    end function is_type_specification

    ! Skip to end of current line
    subroutine skip_to_end_of_line(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()  ! consume newline
                exit
            end if
            token = parser%consume()
        end do
    end subroutine skip_to_end_of_line

end module parser_utilities