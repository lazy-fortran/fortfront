module lexer_core
    use error_handling
    implicit none
    private

    ! Token types
    integer, parameter, public :: TK_EOF = 0
    integer, parameter, public :: TK_IDENTIFIER = 1
    integer, parameter, public :: TK_NUMBER = 2
    integer, parameter, public :: TK_STRING = 3
    integer, parameter, public :: TK_OPERATOR = 4
    integer, parameter, public :: TK_KEYWORD = 5
    integer, parameter, public :: TK_NEWLINE = 6
    integer, parameter, public :: TK_COMMENT = 7
    integer, parameter, public :: TK_WHITESPACE = 8    ! New: for CST trivia
    integer, parameter, public :: TK_UNKNOWN = 99
    
    ! Backward compatibility aliases
    integer, parameter, public :: TOKEN_WHITESPACE = TK_WHITESPACE
    integer, parameter, public :: TOKEN_COMMENT = TK_COMMENT
    integer, parameter, public :: TOKEN_NEWLINE = TK_NEWLINE

    ! Trivia token type (simpler, non-recursive)
    type, public :: trivia_token_t
        integer :: kind = TK_UNKNOWN
        character(len=:), allocatable :: text
        integer :: line = 1
        integer :: column = 1
    end type trivia_token_t

    ! Token structure
    type, public :: token_t
        integer :: kind = TK_UNKNOWN
        character(len=:), allocatable :: text
        integer :: line = 1
        integer :: column = 1
        ! CST trivia storage (optional) - using separate type to avoid recursion
        type(trivia_token_t), allocatable :: leading_trivia(:)
        type(trivia_token_t), allocatable :: trailing_trivia(:)
    contains
        procedure :: deep_copy => token_deep_copy
        procedure :: assign => token_assign
        generic :: assignment(=) => assign
    end type token_t

    ! Lexer options for configurable behavior
    type, public :: lexer_options_t
        logical :: collect_trivia = .false.        ! Default: no trivia collection
        logical :: preserve_newlines = .false.     ! Preserve newlines as tokens
        logical :: preserve_whitespace = .false.   ! Preserve whitespace as tokens
        logical :: preserve_comments = .true.      ! Always preserve comments
    contains
        procedure :: with_trivia => options_with_trivia
        procedure :: default => options_default
    end type lexer_options_t

    ! Lexer result types
    type, public :: tokenize_result_t
        type(result_t) :: result
        type(token_t), allocatable :: tokens(:)
        integer :: token_count = 0
    end type

    ! Public interface
    public :: tokenize_core, tokenize_safe
    public :: tokenize_with_options, tokenize_safe_with_options
    public :: token_type_name

    ! Keywords list
    character(len=20), dimension(56) :: keywords = [ &
                       "program     ", "end         ", "function    ", "subroutine  ", &
                       "if          ", "then        ", "else        ", "endif       ", &
                       "do          ", "while       ", "implicit    ", "none        ", &
                       "integer     ", "real        ", "logical     ", "character   ", &
                       "complex     ", "print       ", "read        ", "write       ", &
                       "call        ", "use         ", "select      ", "case        ", &
                                       "default     ", "type        ", "interface   ", &
                                       "operator    ", "module      ", "contains    ", &
                                       "only        ", "include     ", "elseif      ", &
                                        "assignment  ", "intent      ", &
                                        "in          ", &
                                        "out         ", "inout       ", &
                                        "stop        ", "go          ", "to          ", "error       ", &
                                        "return      ", "cycle       ", &
                                        "exit        ", &
                                        "where       ", "elsewhere   ", &
                                        "optional    ", &
                                        "present     ", "open        ", &
                                        "close       ", &
                                        "parameter   ", "allocate    ", &
                                        "deallocate  ", &
                                        "forall      ", "associate   " &
                                        ]

contains

    function tokenize_safe(source) result(tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t) :: tokenize_res
        
        ! Input validation
        if (len(source) == 0) then
            tokenize_res%result = warning_result( &
                "Empty source provided to tokenizer", &
                ERROR_VALIDATION, &
                component="lexer", &
                context="tokenize_safe", &
                suggestion="Provide non-empty source text")
            tokenize_res%token_count = 0
            return
        end if
        
        ! Delegate to core implementation with error handling
        call tokenize_core_safe(source, tokenize_res)
    end function tokenize_safe

    subroutine tokenize_core(source, tokens)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)

        type(token_t), allocatable :: temp_tokens(:)
        integer :: pos, line_num, col_num, token_count
        integer :: source_len
        character(len=1) :: ch

        ! Initialize
        source_len = len(source)
        pos = 1
        line_num = 1
        col_num = 1
        token_count = 0
        allocate (temp_tokens(100))  ! Initial allocation

        ! Main tokenization loop
        do while (pos <= source_len)
            ch = source(pos:pos)

            ! Skip whitespace
            if (is_whitespace(ch)) then
                if (ch == new_line('a')) then
                    line_num = line_num + 1
                    col_num = 1
                else
                    col_num = col_num + 1
                end if
                pos = pos + 1
                cycle
            end if

            ! Number literal
            if (is_digit(ch)) then
              call scan_number(source, pos, line_num, col_num, temp_tokens, token_count)

                ! String literal
            else if (ch == '"' .or. ch == "'") then
              call scan_string(source, pos, line_num, col_num, temp_tokens, token_count)

                ! Identifier or keyword
            else if (is_letter(ch)) then
          call scan_identifier(source, pos, line_num, col_num, temp_tokens, token_count)

                ! Comments - capture and emit as tokens
            else if (ch == '!') then
                call scan_comment(source, pos, line_num, col_num, &
                    temp_tokens, token_count)

                ! Logical constants and operators (starting with '.')
            else if (ch == '.') then
       call scan_logical_token(source, pos, line_num, col_num, temp_tokens, token_count)

                ! Operators
            else if (is_operator(ch)) then
            call scan_operator(source, pos, line_num, col_num, temp_tokens, token_count)

                ! Unknown character
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do

        ! Add EOF token
        token_count = token_count + 1
        if (token_count > size(temp_tokens)) then
            call resize_tokens(temp_tokens)
        end if
        temp_tokens(token_count)%kind = TK_EOF
        temp_tokens(token_count)%text = ""
        temp_tokens(token_count)%line = line_num
        temp_tokens(token_count)%column = col_num

        ! Copy to output array
        allocate (tokens(token_count))
        tokens = temp_tokens(1:token_count)

    end subroutine tokenize_core

    subroutine tokenize_core_safe(source, tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t), intent(out) :: tokenize_res

        type(token_t), allocatable :: temp_tokens(:)
        integer :: pos, line_num, col_num, token_count
        integer :: source_len
        character(len=1) :: ch
        type(result_t) :: scan_result

        ! Initialize success result
        tokenize_res%result = success_result()
        
        ! Initialize
        source_len = len(source)
        pos = 1
        line_num = 1
        col_num = 1
        token_count = 0
        allocate (temp_tokens(100))  ! Initial allocation

        ! Main tokenization loop
        do while (pos <= source_len)
            ch = source(pos:pos)

            ! Skip whitespace
            if (is_whitespace(ch)) then
                if (ch == new_line('a')) then
                    line_num = line_num + 1
                    col_num = 1
                else
                    col_num = col_num + 1
                end if
                pos = pos + 1
                cycle
            end if

            ! Number literal
            if (is_digit(ch)) then
                call scan_number_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! String literal
            else if (ch == '"' .or. ch == "'") then
                call scan_string_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! Identifier or keyword
            else if (is_letter(ch)) then
                call scan_identifier_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! Comments - capture and emit as tokens
            else if (ch == '!') then
                call scan_comment_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! Logical constants and operators (starting with '.')
            else if (ch == '.') then
                call scan_logical_token_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! Operators
            else if (is_operator(ch)) then
                call scan_operator_safe(source, pos, line_num, col_num, temp_tokens, token_count, scan_result)
                if (scan_result%is_failure()) then
                    tokenize_res%result = scan_result
                    return
                end if

            ! Unknown character - now properly handled as error
            else
                tokenize_res%result = create_error_result( &
                    "Invalid character '" // ch // "' in source", &
                    ERROR_VALIDATION, &
                    component="lexer", &
                    context="tokenize", &
                    suggestion="Remove or replace invalid character")
                return
            end if
        end do

        ! Add EOF token
        token_count = token_count + 1
        if (token_count > size(temp_tokens)) then
            call resize_tokens(temp_tokens)
        end if
        temp_tokens(token_count)%kind = TK_EOF
        temp_tokens(token_count)%text = ""
        temp_tokens(token_count)%line = line_num
        temp_tokens(token_count)%column = col_num

        ! Copy to output
        tokenize_res%token_count = token_count
        allocate(tokenize_res%tokens(token_count))
        tokenize_res%tokens = temp_tokens(1:token_count)

    end subroutine tokenize_core_safe

    subroutine scan_number(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)

        integer :: start_pos, start_col
        logical :: has_dot

        start_pos = pos
        start_col = col_num
        has_dot = .false.

        ! Scan integer part
        do while (pos <= len(source))
            if (.not. is_digit(source(pos:pos))) exit
            pos = pos + 1
            col_num = col_num + 1
        end do

        ! Check for decimal point
        if (pos <= len(source)) then
            if (source(pos:pos) == '.') then
                has_dot = .true.
                pos = pos + 1
                col_num = col_num + 1

                ! Scan fractional part
                do while (pos <= len(source))
                    if (.not. is_digit(source(pos:pos))) exit
                    pos = pos + 1
                    col_num = col_num + 1
                end do
            end if
        end if

        ! Check for exponent
        if (pos <= len(source)) then
            if (source(pos:pos) == 'e' .or. source(pos:pos) == 'E' .or. &
                source(pos:pos) == 'd' .or. source(pos:pos) == 'D') then
                pos = pos + 1
                col_num = col_num + 1

                ! Optional sign
                if (pos <= len(source)) then
                    if (source(pos:pos) == '+' .or. source(pos:pos) == '-') then
                        pos = pos + 1
                        col_num = col_num + 1
                    end if
                end if

                ! Exponent digits
                do while (pos <= len(source))
                    if (.not. is_digit(source(pos:pos))) exit
                    pos = pos + 1
                    col_num = col_num + 1
                end do
            end if
        end if

        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_NUMBER
        tokens(token_count)%text = source(start_pos:pos - 1)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

    end subroutine scan_number

    subroutine scan_comment(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        character(len=:), allocatable :: comment_text

        start_pos = pos
        start_col = col_num
        
        ! Skip the ! character
        pos = pos + 1
        col_num = col_num + 1

        ! Collect comment text until end of line or end of source
        comment_text = "!"
        do while (pos <= len(source))
            if (source(pos:pos) == new_line('a')) exit
            comment_text = comment_text // source(pos:pos)
            pos = pos + 1
            col_num = col_num + 1
        end do

        ! Create comment token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_COMMENT
        tokens(token_count)%text = comment_text
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col
    end subroutine scan_comment

    subroutine scan_string(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)

        integer :: start_pos, start_col
        character(len=1) :: quote_char

        start_pos = pos
        start_col = col_num
        quote_char = source(pos:pos)

        pos = pos + 1
        col_num = col_num + 1

        ! Scan until closing quote
        do while (pos <= len(source))
            if (source(pos:pos) == quote_char) then
                ! Check if it's an escaped quote (doubled)
                if (pos + 1 <= len(source)) then
                    if (source(pos + 1:pos + 1) == quote_char) then
                        ! Escaped quote - skip both characters
                        pos = pos + 2
                        col_num = col_num + 2
                    else
                        ! End of string
                        pos = pos + 1
                        col_num = col_num + 1
                        exit
                    end if
                else
                    ! End of string
                    pos = pos + 1
                    col_num = col_num + 1
                    exit
                end if
            else if (source(pos:pos) == new_line('a')) then
                ! Error: unterminated string
                exit
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do

        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_STRING
        tokens(token_count)%text = source(start_pos:pos - 1)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

    end subroutine scan_string

    subroutine scan_string_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        integer :: start_pos, start_col
        character(len=1) :: quote_char
        logical :: found_end

        scan_result = success_result()
        start_pos = pos
        start_col = col_num
        quote_char = source(pos:pos)
        found_end = .false.

        pos = pos + 1
        col_num = col_num + 1

        ! Scan until closing quote
        do while (pos <= len(source))
            if (source(pos:pos) == quote_char) then
                ! Check if it's an escaped quote (doubled)
                if (pos + 1 <= len(source)) then
                    if (source(pos + 1:pos + 1) == quote_char) then
                        ! Escaped quote - skip both characters
                        pos = pos + 2
                        col_num = col_num + 2
                    else
                        ! End of string
                        pos = pos + 1
                        col_num = col_num + 1
                        found_end = .true.
                        exit
                    end if
                else
                    ! End of string
                    pos = pos + 1
                    col_num = col_num + 1
                    found_end = .true.
                    exit
                end if
            else if (source(pos:pos) == new_line('a')) then
                ! Error: unterminated string
                scan_result = create_error_result( &
                    "Unterminated string literal", &
                    ERROR_PARSER, &
                    component="lexer", &
                    context="scan_string", &
                    suggestion="Add closing " // quote_char // " to complete string")
                return
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do

        ! Check if we reached end without finding closing quote
        if (.not. found_end) then
            scan_result = create_error_result( &
                "Unterminated string literal at end of source", &
                ERROR_PARSER, &
                component="lexer", &
                context="scan_string", &
                suggestion="Add closing " // quote_char // " to complete string")
            return
        end if

        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_STRING
        tokens(token_count)%text = source(start_pos:pos - 1)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

    end subroutine scan_string_safe

    subroutine scan_identifier(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)

        integer :: start_pos, start_col
        character(len=:), allocatable :: word

        start_pos = pos
        start_col = col_num

        ! Scan identifier
        do while (pos <= len(source))
            if (.not. (is_letter(source(pos:pos)) .or. &
                       is_digit(source(pos:pos)) .or. &
                       source(pos:pos) == '_')) exit
            pos = pos + 1
            col_num = col_num + 1
        end do

        word = source(start_pos:pos - 1)

        ! Add token
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if

        ! Check if it's a keyword
        if (is_keyword(word)) then
            tokens(token_count)%kind = TK_KEYWORD
        else
            tokens(token_count)%kind = TK_IDENTIFIER
        end if

        tokens(token_count)%text = word
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

    end subroutine scan_identifier

    subroutine scan_operator(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)

        integer :: start_col
        character(len=2) :: two_char

        start_col = col_num

        ! Check for two-character operators
        if (pos < len(source)) then
            two_char = source(pos:pos + 1)
            if (two_char == "==" .or. two_char == "/=" .or. &
                two_char == "<=" .or. two_char == ">=" .or. &
                two_char == "::" .or. two_char == "**" .or. &
                two_char == "//" .or. two_char == "=>") then

                token_count = token_count + 1
                if (token_count > size(tokens)) then
                    call resize_tokens(tokens)
                end if
                tokens(token_count)%kind = TK_OPERATOR
                tokens(token_count)%text = two_char
                tokens(token_count)%line = line_num
                tokens(token_count)%column = start_col

                pos = pos + 2
                col_num = col_num + 2
                return
            end if
        end if

        ! Single character operator
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_OPERATOR
        tokens(token_count)%text = source(pos:pos)
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

        pos = pos + 1
        col_num = col_num + 1

    end subroutine scan_operator

    subroutine scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)

        integer :: start_pos, start_col, remaining
        character(len=:), allocatable :: word

        start_pos = pos
        start_col = col_num

        ! Check for logical constants and operators that start with '.'
        if (pos <= len(source)) then
            ! Calculate remaining characters
            remaining = len(source) - pos + 1

            ! Check for .true. (6 characters)
            if (remaining >= 6) then
                if (source(pos:pos + 5) == ".true.") then
                    ! Found .true.
                    pos = pos + 6
                    col_num = col_num + 6
                    word = ".true."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_KEYWORD
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if

            ! Check for .false. (7 characters)
            if (remaining >= 7) then
                if (source(pos:pos + 6) == ".false.") then
                    ! Found .false.
                    pos = pos + 7
                    col_num = col_num + 7
                    word = ".false."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_KEYWORD
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if

            ! Check for .and. and .not. (5 characters)
            if (remaining >= 5) then
                if (source(pos:pos + 4) == ".and.") then
                    ! Found .and.
                    pos = pos + 5
                    col_num = col_num + 5
                    word = ".and."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_OPERATOR
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                else if (source(pos:pos + 4) == ".not.") then
                    ! Found .not.
                    pos = pos + 5
                    col_num = col_num + 5
                    word = ".not."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_OPERATOR
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if

            ! Check for .eqv. and .neqv. (5 and 6 characters)
            if (remaining >= 6) then
                if (source(pos:pos + 5) == ".neqv.") then
                    ! Found .neqv.
                    pos = pos + 6
                    col_num = col_num + 6
                    word = ".neqv."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_OPERATOR
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if

            if (remaining >= 5) then
                if (source(pos:pos + 4) == ".eqv.") then
                    ! Found .eqv.
                    pos = pos + 5
                    col_num = col_num + 5
                    word = ".eqv."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_OPERATOR
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if

            ! Check for .or. (4 characters)
            if (remaining >= 4) then
                if (source(pos:pos + 3) == ".or.") then
                    ! Found .or.
                    pos = pos + 4
                    col_num = col_num + 4
                    word = ".or."

                    token_count = token_count + 1
                    if (token_count > size(tokens)) then
                        call resize_tokens(tokens)
                    end if
                    tokens(token_count)%kind = TK_OPERATOR
                    tokens(token_count)%text = word
                    tokens(token_count)%line = line_num
                    tokens(token_count)%column = start_col
                    return
                end if
            end if
        end if

        ! If we get here, it's just a regular '.' operator
        token_count = token_count + 1
        if (token_count > size(tokens)) then
            call resize_tokens(tokens)
        end if
        tokens(token_count)%kind = TK_OPERATOR
        tokens(token_count)%text = "."
        tokens(token_count)%line = line_num
        tokens(token_count)%column = start_col

        pos = pos + 1
        col_num = col_num + 1

    end subroutine scan_logical_token

    ! Safe scanning functions with proper error handling

    subroutine scan_number_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        ! Numbers are generally robust, delegate to existing implementation
        scan_result = success_result()
        call scan_number(source, pos, line_num, col_num, tokens, token_count)
    end subroutine scan_number_safe

    subroutine scan_identifier_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        ! Identifiers are generally robust, delegate to existing implementation
        scan_result = success_result()
        call scan_identifier(source, pos, line_num, col_num, tokens, token_count)
    end subroutine scan_identifier_safe

    subroutine scan_comment_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        ! Comments are generally robust, delegate to existing implementation
        scan_result = success_result()
        call scan_comment(source, pos, line_num, col_num, tokens, token_count)
    end subroutine scan_comment_safe

    subroutine scan_operator_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        ! Operators are generally robust, delegate to existing implementation
        scan_result = success_result()
        call scan_operator(source, pos, line_num, col_num, tokens, token_count)
    end subroutine scan_operator_safe

    subroutine scan_logical_token_safe(source, pos, line_num, col_num, tokens, token_count, scan_result)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(result_t), intent(out) :: scan_result

        ! Logical tokens are generally robust, delegate to existing implementation
        scan_result = success_result()
        call scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
    end subroutine scan_logical_token_safe

    ! Helper functions

    logical function is_whitespace(ch)
        character(len=1), intent(in) :: ch
        is_whitespace = (ch == ' ' .or. ch == char(9) .or. ch == new_line('a'))
    end function is_whitespace

    logical function is_letter(ch)
        character(len=1), intent(in) :: ch
        is_letter = (ch >= 'A' .and. ch <= 'Z') .or. (ch >= 'a' .and. ch <= 'z')
    end function is_letter

    logical function is_digit(ch)
        character(len=1), intent(in) :: ch
        is_digit = (ch >= '0' .and. ch <= '9')
    end function is_digit

    logical function is_operator(ch)
        character(len=1), intent(in) :: ch
        is_operator = index("+-*/=<>()[]{},:;%", ch) > 0
    end function is_operator

    logical function is_keyword(word)
        character(len=*), intent(in) :: word
        integer :: i
        character(len=20) :: lower_word

        ! Convert to lowercase for comparison
        lower_word = to_lower(word)

        do i = 1, size(keywords)
            if (trim(lower_word) == trim(keywords(i))) then
                is_keyword = .true.
                return
            end if
        end do

        is_keyword = .false.
    end function is_keyword

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        do i = 1, len(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                lower_str(i:i) = char(ichar(str(i:i)) + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
    end function to_lower

    subroutine resize_tokens(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), allocatable :: temp(:)

        allocate (temp(size(tokens)*2))
        temp(1:size(tokens)) = tokens
        ! Use move_alloc for O(1) array transfer instead of O(n) copying
        call move_alloc(temp, tokens)
    end subroutine resize_tokens

    subroutine resize_trivia_buffer(trivia)
        type(trivia_token_t), allocatable, intent(inout) :: trivia(:)
        type(trivia_token_t), allocatable :: temp(:)

        allocate (temp(size(trivia)*2))
        temp(1:size(trivia)) = trivia
        ! Use move_alloc for O(1) array transfer instead of O(n) copying
        call move_alloc(temp, trivia)
    end subroutine resize_trivia_buffer

    ! Convert token type to string name
    function token_type_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=:), allocatable :: name

        select case (kind)
        case (TK_EOF)
            name = "eof"
        case (TK_IDENTIFIER)
            name = "identifier"
        case (TK_NUMBER)
            name = "number"
        case (TK_STRING)
            name = "string"
        case (TK_OPERATOR)
            name = "operator"
        case (TK_KEYWORD)
            name = "keyword"
        case (TK_NEWLINE)
            name = "newline"
        case (TK_COMMENT)
            name = "comment"
        case (TK_WHITESPACE)
            name = "whitespace"
        case default
            name = "unknown"
        end select
    end function token_type_name

    ! Deep copy procedures for token_t
    function token_deep_copy(this) result(copy)
        class(token_t), intent(in) :: this
        type(token_t) :: copy
        
        copy%kind = this%kind
        copy%line = this%line
        copy%column = this%column
        
        if (allocated(this%text)) then
            copy%text = this%text
        end if
        
        ! Deep copy trivia arrays if present (simple copy for trivia_token_t)
        if (allocated(this%leading_trivia)) then
            allocate(copy%leading_trivia(size(this%leading_trivia)))
            copy%leading_trivia = this%leading_trivia
        end if
        
        if (allocated(this%trailing_trivia)) then
            allocate(copy%trailing_trivia(size(this%trailing_trivia)))
            copy%trailing_trivia = this%trailing_trivia
        end if
    end function token_deep_copy

    subroutine token_assign(lhs, rhs)
        class(token_t), intent(out) :: lhs
        type(token_t), intent(in) :: rhs

        lhs%kind = rhs%kind
        lhs%line = rhs%line
        lhs%column = rhs%column

        if (allocated(rhs%text)) then
            lhs%text = rhs%text
        end if
        
        ! Deep copy trivia arrays if present
        if (allocated(rhs%leading_trivia)) then
            allocate(lhs%leading_trivia(size(rhs%leading_trivia)))
            lhs%leading_trivia = rhs%leading_trivia
        end if
        
        if (allocated(rhs%trailing_trivia)) then
            allocate(lhs%trailing_trivia(size(rhs%trailing_trivia)))
            lhs%trailing_trivia = rhs%trailing_trivia
        end if
    end subroutine token_assign

    ! Lexer options methods
    function options_with_trivia(this) result(opts)
        class(lexer_options_t), intent(in) :: this
        type(lexer_options_t) :: opts
        
        opts = this
        opts%collect_trivia = .true.
        opts%preserve_newlines = .true.
        opts%preserve_whitespace = .true.
        opts%preserve_comments = .true.
    end function options_with_trivia
    
    function options_default(this) result(opts)
        class(lexer_options_t), intent(in) :: this
        type(lexer_options_t) :: opts
        
        opts%collect_trivia = .false.
        opts%preserve_newlines = .false.
        opts%preserve_whitespace = .false.
        opts%preserve_comments = .true.
    end function options_default

    ! New tokenize functions with options
    subroutine tokenize_with_options(source, tokens, options)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(lexer_options_t), intent(in) :: options

        type(token_t), allocatable :: temp_tokens(:)
        type(trivia_token_t), allocatable :: trivia_buffer(:)
        integer :: pos, line_num, col_num, token_count, trivia_count
        integer :: source_len
        character(len=1) :: ch

        ! Initialize
        source_len = len(source)
        pos = 1
        line_num = 1
        col_num = 1
        token_count = 0
        trivia_count = 0
        allocate (temp_tokens(100))
        allocate (trivia_buffer(20))

        ! Main tokenization loop
        do while (pos <= source_len)
            ch = source(pos:pos)

            ! Handle whitespace based on options
            if (is_whitespace(ch)) then
                if (ch == new_line('a')) then
                    if (options%preserve_newlines .and. options%collect_trivia) then
                        call add_trivia_token(source, pos, line_num, col_num, &
                            TK_NEWLINE, trivia_buffer, trivia_count)
                    end if
                    line_num = line_num + 1
                    col_num = 1
                    pos = pos + 1
                else if (options%preserve_whitespace .and. options%collect_trivia) then
                    call scan_whitespace(source, pos, line_num, col_num, &
                        trivia_buffer, trivia_count)
                else
                    col_num = col_num + 1
                    pos = pos + 1
                end if
                cycle
            end if

            ! Comments - handle based on options
            if (ch == '!') then
                if (options%collect_trivia) then
                    call scan_comment_trivia(source, pos, line_num, col_num, &
                        trivia_buffer, trivia_count)
                else
                    call scan_comment(source, pos, line_num, col_num, &
                        temp_tokens, token_count)
                end if
                cycle
            end if

            ! Attach leading trivia to next significant token
            if (trivia_count > 0 .and. options%collect_trivia) then
                ! The next token will get the accumulated trivia
            end if

            ! Number literal
            if (is_digit(ch)) then
                call scan_number_with_trivia(source, pos, line_num, col_num, &
                    temp_tokens, token_count, trivia_buffer, trivia_count, options)

            ! String literal
            else if (ch == '"' .or. ch == "'") then
                call scan_string_with_trivia(source, pos, line_num, col_num, &
                    temp_tokens, token_count, trivia_buffer, trivia_count, options)

            ! Identifier or keyword
            else if (is_letter(ch)) then
                call scan_identifier_with_trivia(source, pos, line_num, col_num, &
                    temp_tokens, token_count, trivia_buffer, trivia_count, options)

            ! Logical constants and operators (starting with '.')
            else if (ch == '.') then
                call scan_logical_token_with_trivia(source, pos, line_num, col_num, &
                    temp_tokens, token_count, trivia_buffer, trivia_count, options)

            ! Operators
            else if (is_operator(ch)) then
                call scan_operator_with_trivia(source, pos, line_num, col_num, &
                    temp_tokens, token_count, trivia_buffer, trivia_count, options)

            ! Unknown character
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end do

        ! Add EOF token
        token_count = token_count + 1
        if (token_count > size(temp_tokens)) then
            call resize_tokens(temp_tokens)
        end if
        temp_tokens(token_count)%kind = TK_EOF
        temp_tokens(token_count)%text = ""
        temp_tokens(token_count)%line = line_num
        temp_tokens(token_count)%column = col_num

        ! Copy to output array
        allocate (tokens(token_count))
        tokens = temp_tokens(1:token_count)

    end subroutine tokenize_with_options

    function tokenize_safe_with_options(source, options) result(tokenize_res)
        character(len=*), intent(in) :: source
        type(lexer_options_t), intent(in) :: options
        type(tokenize_result_t) :: tokenize_res
        
        ! Input validation
        if (len(source) == 0) then
            tokenize_res%result = warning_result( &
                "Empty source provided to tokenizer", &
                ERROR_VALIDATION, &
                component="lexer", &
                context="tokenize_safe_with_options", &
                suggestion="Provide non-empty source text")
            tokenize_res%token_count = 0
            return
        end if
        
        ! Delegate to implementation
        call tokenize_with_options(source, tokenize_res%tokens, options)
        if (allocated(tokenize_res%tokens)) then
            tokenize_res%token_count = size(tokenize_res%tokens)
        else
            tokenize_res%token_count = 0
        end if
        tokenize_res%result = success_result()
    end function tokenize_safe_with_options

    ! Helper function to scan whitespace as trivia
    subroutine scan_whitespace(source, pos, line_num, col_num, trivia_buffer, trivia_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, col_num, trivia_count
        integer, intent(in) :: line_num
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        
        integer :: start_pos, start_col
        character(len=:), allocatable :: ws_text
        
        start_pos = pos
        start_col = col_num
        ws_text = ""
        
        ! Collect consecutive whitespace (not newlines)
        do while (pos <= len(source))
            if (.not. is_whitespace(source(pos:pos))) exit
            if (source(pos:pos) == new_line('a')) exit
            ws_text = ws_text // source(pos:pos)
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        ! Add whitespace token to trivia buffer
        trivia_count = trivia_count + 1
        if (trivia_count > size(trivia_buffer)) then
            call resize_trivia_buffer(trivia_buffer)
        end if
        trivia_buffer(trivia_count)%kind = TK_WHITESPACE
        trivia_buffer(trivia_count)%text = ws_text
        trivia_buffer(trivia_count)%line = line_num
        trivia_buffer(trivia_count)%column = start_col
    end subroutine scan_whitespace

    ! Helper to scan comment as trivia
    subroutine scan_comment_trivia(source, pos, line_num, col_num, trivia_buffer, trivia_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, col_num, trivia_count
        integer, intent(in) :: line_num
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        
        integer :: start_pos, start_col
        character(len=:), allocatable :: comment_text
        
        start_pos = pos
        start_col = col_num
        
        ! Skip the ! character
        pos = pos + 1
        col_num = col_num + 1
        
        ! Collect comment text until end of line
        comment_text = "!"
        do while (pos <= len(source))
            if (source(pos:pos) == new_line('a')) exit
            comment_text = comment_text // source(pos:pos)
            pos = pos + 1
            col_num = col_num + 1
        end do
        
        ! Add comment token to trivia buffer
        trivia_count = trivia_count + 1
        if (trivia_count > size(trivia_buffer)) then
            call resize_trivia_buffer(trivia_buffer)
        end if
        trivia_buffer(trivia_count)%kind = TK_COMMENT
        trivia_buffer(trivia_count)%text = comment_text
        trivia_buffer(trivia_count)%line = line_num
        trivia_buffer(trivia_count)%column = start_col
    end subroutine scan_comment_trivia

    ! Helper to add a single character trivia token
    subroutine add_trivia_token(source, pos, line_num, col_num, kind, trivia_buffer, trivia_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, col_num, trivia_count
        integer, intent(in) :: line_num, kind
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        
        trivia_count = trivia_count + 1
        if (trivia_count > size(trivia_buffer)) then
            call resize_trivia_buffer(trivia_buffer)
        end if
        trivia_buffer(trivia_count)%kind = kind
        trivia_buffer(trivia_count)%text = source(pos:pos)
        trivia_buffer(trivia_count)%line = line_num
        trivia_buffer(trivia_count)%column = col_num
    end subroutine add_trivia_token

    ! Wrapper functions that attach trivia to tokens
    subroutine scan_number_with_trivia(source, pos, line_num, col_num, tokens, &
            token_count, trivia_buffer, trivia_count, options)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count, trivia_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        type(lexer_options_t), intent(in) :: options
        
        integer :: old_count
        
        old_count = token_count
        call scan_number(source, pos, line_num, col_num, tokens, token_count)
        
        ! Attach leading trivia if any
        if (trivia_count > 0 .and. options%collect_trivia .and. token_count > old_count) then
            allocate(tokens(token_count)%leading_trivia(trivia_count))
            tokens(token_count)%leading_trivia(1:trivia_count) = trivia_buffer(1:trivia_count)
            trivia_count = 0
        end if
    end subroutine scan_number_with_trivia

    subroutine scan_string_with_trivia(source, pos, line_num, col_num, tokens, &
            token_count, trivia_buffer, trivia_count, options)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count, trivia_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        type(lexer_options_t), intent(in) :: options
        
        integer :: old_count
        
        old_count = token_count
        call scan_string(source, pos, line_num, col_num, tokens, token_count)
        
        ! Attach leading trivia if any
        if (trivia_count > 0 .and. options%collect_trivia .and. token_count > old_count) then
            allocate(tokens(token_count)%leading_trivia(trivia_count))
            tokens(token_count)%leading_trivia(1:trivia_count) = trivia_buffer(1:trivia_count)
            trivia_count = 0
        end if
    end subroutine scan_string_with_trivia

    subroutine scan_identifier_with_trivia(source, pos, line_num, col_num, tokens, &
            token_count, trivia_buffer, trivia_count, options)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count, trivia_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        type(lexer_options_t), intent(in) :: options
        
        integer :: old_count
        
        old_count = token_count
        call scan_identifier(source, pos, line_num, col_num, tokens, token_count)
        
        ! Attach leading trivia if any
        if (trivia_count > 0 .and. options%collect_trivia .and. token_count > old_count) then
            allocate(tokens(token_count)%leading_trivia(trivia_count))
            tokens(token_count)%leading_trivia(1:trivia_count) = trivia_buffer(1:trivia_count)
            trivia_count = 0
        end if
    end subroutine scan_identifier_with_trivia

    subroutine scan_logical_token_with_trivia(source, pos, line_num, col_num, tokens, &
            token_count, trivia_buffer, trivia_count, options)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count, trivia_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        type(lexer_options_t), intent(in) :: options
        
        integer :: old_count
        
        old_count = token_count
        call scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
        
        ! Attach leading trivia if any
        if (trivia_count > 0 .and. options%collect_trivia .and. token_count > old_count) then
            allocate(tokens(token_count)%leading_trivia(trivia_count))
            tokens(token_count)%leading_trivia(1:trivia_count) = trivia_buffer(1:trivia_count)
            trivia_count = 0
        end if
    end subroutine scan_logical_token_with_trivia

    subroutine scan_operator_with_trivia(source, pos, line_num, col_num, tokens, &
            token_count, trivia_buffer, trivia_count, options)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count, trivia_count
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: trivia_buffer(:)
        type(lexer_options_t), intent(in) :: options
        
        integer :: old_count
        
        old_count = token_count
        call scan_operator(source, pos, line_num, col_num, tokens, token_count)
        
        ! Attach leading trivia if any
        if (trivia_count > 0 .and. options%collect_trivia .and. token_count > old_count) then
            allocate(tokens(token_count)%leading_trivia(trivia_count))
            tokens(token_count)%leading_trivia(1:trivia_count) = trivia_buffer(1:trivia_count)
            trivia_count = 0
        end if
    end subroutine scan_operator_with_trivia

end module lexer_core
