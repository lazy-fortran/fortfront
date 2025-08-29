module lexer_scanners
    use lexer_token_types
    use error_handling
    implicit none
    private

    ! Public scanning functions
    public :: scan_number, scan_comment, scan_string, scan_identifier, scan_operator, scan_logical_token
    public :: scan_number_safe, scan_comment_safe, scan_string_safe, scan_identifier_safe
    public :: scan_operator_safe, scan_logical_token_safe

contains

    ! Scan a number token
    subroutine scan_number(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col, dot_count, e_count
        logical :: has_dot, has_exp
        character :: c
        
        start_pos = pos
        start_col = col_num
        dot_count = 0
        e_count = 0
        has_dot = .false.
        has_exp = .false.
        
        ! Scan digits, decimal points, and scientific notation
        do while (pos <= len(source))
            c = source(pos:pos)
            
            if (c >= '0' .and. c <= '9') then
                pos = pos + 1
                col_num = col_num + 1
            else if (c == '.' .and. .not. has_dot) then
                has_dot = .true.
                dot_count = 1
                pos = pos + 1
                col_num = col_num + 1
            else if ((c == 'e' .or. c == 'E') .and. .not. has_exp) then
                has_exp = .true.
                e_count = 1
                pos = pos + 1
                col_num = col_num + 1
                ! Handle optional sign after exponent
                if (pos <= len(source)) then
                    c = source(pos:pos)
                    if (c == '+' .or. c == '-') then
                        pos = pos + 1
                        col_num = col_num + 1
                    end if
                end if
            else
                exit
            end if
        end do
        
        ! Create number token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_NUMBER
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_number

    ! Scan a comment token
    subroutine scan_comment(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        character :: c
        
        start_pos = pos
        start_col = col_num
        
        ! Skip the '!' character
        pos = pos + 1
        col_num = col_num + 1
        
        ! Scan until end of line
        do while (pos <= len(source))
            c = source(pos:pos)
            if (c == char(10) .or. c == char(13)) then
                exit
            end if
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        ! Create comment token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_COMMENT
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_comment

    ! Scan a string token
    subroutine scan_string(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        character :: quote_char, c
        logical :: escaped
        
        start_pos = pos
        start_col = col_num
        quote_char = source(pos:pos)
        escaped = .false.
        
        ! Skip opening quote
        pos = pos + 1
        col_num = col_num + 1
        
        ! Scan until closing quote
        do while (pos <= len(source))
            c = source(pos:pos)
            
            if (escaped) then
                escaped = .false.
            else if (c == '\') then
                escaped = .true.
            else if (c == quote_char) then
                pos = pos + 1
                col_num = col_num + 1
                exit
            end if
            
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        ! Create string token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_STRING
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_string

    ! Safe string scanning with error handling
    subroutine scan_string_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_string(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_string_safe

    ! Scan an identifier token
    subroutine scan_identifier(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        character :: c
        
        start_pos = pos
        start_col = col_num
        
        ! Scan identifier characters
        do while (pos <= len(source))
            c = source(pos:pos)
            if ((c >= 'a' .and. c <= 'z') .or. &
                (c >= 'A' .and. c <= 'Z') .or. &
                (c >= '0' .and. c <= '9') .or. &
                c == '_') then
                pos = pos + 1
                col_num = col_num + 1
            else
                exit
            end if
        end do
        
        ! Create identifier/keyword token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            if (is_keyword(source(start_pos:pos-1))) then
                tokens(token_count)%kind = TK_KEYWORD
            else
                tokens(token_count)%kind = TK_IDENTIFIER
            end if
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_identifier

    ! Scan an operator token
    subroutine scan_operator(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        character :: c
        
        start_pos = pos
        start_col = col_num
        c = source(pos:pos)
        
        ! Handle multi-character operators
        select case (c)
        case ('*')
            pos = pos + 1
            col_num = col_num + 1
            if (pos <= len(source) .and. source(pos:pos) == '*') then
                pos = pos + 1
                col_num = col_num + 1
            end if
        case ('/')
            pos = pos + 1
            col_num = col_num + 1
            ! Check for // (concatenation) or /= (not equal)
            if (pos <= len(source)) then
                if (source(pos:pos) == '/') then
                    pos = pos + 1
                    col_num = col_num + 1
                else if (source(pos:pos) == '=') then
                    pos = pos + 1
                    col_num = col_num + 1
                end if
            end if
        case ('=', '<', '>')
            pos = pos + 1
            col_num = col_num + 1
            if (pos <= len(source) .and. source(pos:pos) == '=') then
                pos = pos + 1
                col_num = col_num + 1
            end if
        case default
            pos = pos + 1
            col_num = col_num + 1
        end select
        
        ! Create operator token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_OPERATOR
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_operator

    ! Scan logical token (.not., .and., .or., etc.)
    subroutine scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col, end_pos
        character :: c
        
        start_pos = pos
        start_col = col_num
        
        ! Skip the first dot
        pos = pos + 1
        col_num = col_num + 1
        
        ! Find the closing dot
        do while (pos <= len(source))
            c = source(pos:pos)
            if (c == '.') then
                pos = pos + 1
                col_num = col_num + 1
                exit
            else if ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')) then
                pos = pos + 1
                col_num = col_num + 1
            else
                exit
            end if
        end do
        
        ! Create operator token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_OPERATOR
            tokens(token_count)%text = source(start_pos:pos-1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_logical_token

    ! Safe scanning functions
    subroutine scan_number_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_number(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_number_safe

    subroutine scan_identifier_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_identifier(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_identifier_safe

    subroutine scan_comment_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_comment(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_comment_safe

    subroutine scan_operator_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_operator(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_operator_safe

    subroutine scan_logical_token_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(scan_result_t), intent(out) :: scan_result
        integer :: start_pos
        
        start_pos = pos
        scan_result%success = .true.
        scan_result%result = success_result()
        
        call scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
        
        scan_result%chars_consumed = pos - start_pos
    end subroutine scan_logical_token_safe

    ! Helper function to check if a word is a keyword
    function is_keyword(word) result(keyword)
        character(len=*), intent(in) :: word
        logical :: keyword
        character(len=len(word)) :: lower_word
        
        lower_word = to_lower(word)
        
        select case (trim(lower_word))
        case ('program', 'end', 'function', 'subroutine', 'if', 'then', 'else', &
              'do', 'while', 'for', 'integer', 'real', 'logical', 'character', &
              'implicit', 'none', 'parameter', 'dimension', 'allocatable', &
              'intent', 'in', 'out', 'inout', 'use', 'module', 'contains', &
              'public', 'private', 'type', 'class', 'extends', 'abstract', &
              'procedure', 'interface', 'generic', 'operator', 'assignment')
            keyword = .true.
        case default
            keyword = .false.
        end select
    end function is_keyword

    ! Helper function to convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then
                lower_str(i:i) = achar(ascii_val + 32)
            end if
        end do
    end function to_lower

end module lexer_scanners