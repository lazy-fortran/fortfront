module lexer_scanners
    use lexer_token_types
    use error_handling
    use string_utils_mod, only: to_lower
    implicit none
    private

    ! Public scanning functions
    public :: scan_number, scan_comment, scan_string, scan_identifier, scan_operator, &
        scan_logical_token
    public :: scan_number_safe, scan_comment_safe, scan_string_safe, &
        scan_identifier_safe
    public :: scan_operator_safe, scan_logical_token_safe
    public :: find_unterminated_character_constant

    ! Public source character classification (F2018 6.1 character set)
    public :: is_legal_source_char, is_name_body_char, is_percent_prefix_char

contains

    ! Scan a number token (including Hollerith constants like 2Hab)
    subroutine scan_number(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col, dot_count, e_count, digit_end_pos
        logical :: has_dot, has_exp
        character :: c

        start_pos = pos
        start_col = col_num
        dot_count = 0
        e_count = 0
        has_dot = .false.
        has_exp = .false.

        ! Scan digits first (needed for Hollerith check)
        do while (pos <= len(source))
            c = source(pos:pos)
            if (c >= '0' .and. c <= '9') then
                pos = pos + 1
                col_num = col_num + 1
            else
                exit
            end if
        end do

        digit_end_pos = pos

        ! Check for Hollerith constant (nH followed by n characters)
        if (pos <= len(source)) then
            c = source(pos:pos)
            if (c == 'H' .or. c == 'h') then
                ! This is a Hollerith constant
                block
                    integer :: hollerith_count, i, iostat_val
                    character(len=:), allocatable :: digits_str

                    digits_str = source(start_pos:digit_end_pos - 1)
                    read (digits_str, *, iostat=iostat_val) hollerith_count

                    if (iostat_val == 0 .and. hollerith_count > 0) then
                        ! Consume the H
                        pos = pos + 1
                        col_num = col_num + 1

                        ! Consume exactly hollerith_count characters
                        do i = 1, hollerith_count
                            if (pos <= len(source)) then
                                c = source(pos:pos)
                                ! Do not stop at newlines in Hollerith
                                if (c == char(10) .or. c == char(13)) exit
                                pos = pos + 1
                                col_num = col_num + 1
                            else
                                exit
                            end if
                        end do

                        ! Create Hollerith token (use STRING kind)
                        if (token_count < size(tokens)) then
                            token_count = token_count + 1
                            tokens(token_count)%kind = TK_STRING
                            tokens(token_count)%text = source(start_pos:pos - 1)
                            tokens(token_count)%line = line_num
                            tokens(token_count)%column = start_col
                        end if
                        return
                    end if
                end block
            end if
        end if

        ! Not a Hollerith constant - continue with decimal/exponent scanning
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
            else if (((c == 'e' .or. c == 'E') .or. (c == 'd' .or. c == 'D')) .and. &
                    .not. has_exp) then
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

        ! Capture optional kind suffix (e.g., _int32, _real64)
        if (pos <= len(source)) then
            c = source(pos:pos)
            if (c == '_') then
                pos = pos + 1
                col_num = col_num + 1
                do while (pos <= len(source))
                    c = source(pos:pos)
                    if ((c >= '0' .and. c <= '9') .or. (c >= 'A' .and. c <= 'Z') .or. &
                        (c >= 'a' .and. c <= 'z') .or. c == '_') then
                        pos = pos + 1
                        col_num = col_num + 1
                    else
                        exit
                    end if
                end do
            end if
        end if

        ! Create number token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_NUMBER
            tokens(token_count)%text = source(start_pos:pos - 1)
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

        ! Skip the ! or # character
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
            tokens(token_count)%text = source(start_pos:pos - 1)
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
        logical :: escaped, found_closing_quote

        start_pos = pos
        start_col = col_num
        quote_char = source(pos:pos)
        escaped = .false.
        found_closing_quote = .false.

        ! Skip opening quote
        pos = pos + 1
        col_num = col_num + 1

        ! Scan until closing quote, end of line, or end of file
        ! In Fortran, doubled quote characters inside strings represent a
        ! literal quote (e.g., cant or cant produces cant)
        do while (pos <= len(source))
            c = source(pos:pos)

            ! Stop at newlines to prevent multiline string literals
            if (c == char(10) .or. c == char(13)) then
                exit
            end if

            if (escaped) then
                escaped = .false.
            else if (c == '\') then
                escaped = .true.
            else if (c == quote_char) then
                ! Check if this is a doubled quote (Fortran escape sequence)
                if (pos + 1 <= len(source)) then
                    if (source(pos + 1:pos + 1) == quote_char) then
                        ! Doubled quote - skip both and continue
                        pos = pos + 2
                        col_num = col_num + 2
                        cycle
                    end if
                end if
                ! Single quote - this is the closing quote
                pos = pos + 1
                col_num = col_num + 1
                found_closing_quote = .true.
                exit
            end if

            pos = pos + 1
            col_num = col_num + 1

            ! Check if weve reached the end - if so, we have an unclosed string
            if (pos > len(source)) then
                exit
            end if
        end do

        ! Create string token - ensure its always valid Fortran
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_STRING
            if (found_closing_quote) then
                ! Complete string token
                tokens(token_count)%text = source(start_pos:pos - 1)
            else
                ! Unclosed string - add a closing quote to keep output valid
                ! Extract content until current position and append the terminator
                tokens(token_count)%text = source(start_pos:pos - 1)//quote_char
            end if
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_string

    ! Safe string scanning with error handling
    subroutine scan_string_safe(source, pos, line_num, col_num, tokens, token_count, &
            scan_result)
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
        character(len=:), allocatable :: token_text

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
            token_text = source(start_pos:pos - 1)
            if (is_keyword(token_text)) then
                tokens(token_count)%kind = TK_KEYWORD
            else
                tokens(token_count)%kind = TK_IDENTIFIER
            end if
            tokens(token_count)%text = token_text
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
            if (pos <= len(source)) then
                if (source(pos:pos) == '*') then
                    pos = pos + 1
                    col_num = col_num + 1
                end if
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
            ! Guard evaluation order to avoid out-of-bounds when at end of line
            if (pos <= len(source)) then
                select case (c)
                case ('=')
                    if (source(pos:pos) == '=' .or. source(pos:pos) == '>') then
                        pos = pos + 1
                        col_num = col_num + 1
                    end if
                case ('<', '>')
                    if (source(pos:pos) == '=') then
                        pos = pos + 1
                        col_num = col_num + 1
                    end if
                end select
            end if
        case (':')
            pos = pos + 1
            col_num = col_num + 1
            if (pos <= len(source)) then
                if (source(pos:pos) == ':' .or. source(pos:pos) == '=') then
                    pos = pos + 1
                    col_num = col_num + 1
                end if
            end if
        case default
            pos = pos + 1
            col_num = col_num + 1
        end select

        ! Create operator token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_OPERATOR
            tokens(token_count)%text = source(start_pos:pos - 1)
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_operator

    ! Scan logical token (.not., .and., .or., .true., .false., etc.)
    subroutine scan_logical_token(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col, end_pos
        character :: c
        character(len=:), allocatable :: token_text

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

        ! A logical literal may carry a kind selector immediately after its
        ! closing dot, for example .true._1 or .false._logical_kind. Keep the
        ! selector in the same token so semantic metadata can resolve it.
        token_text = source(start_pos:pos - 1)
        if (is_logical_constant(token_text)) then
            if (pos <= len(source)) then
                if (source(pos:pos) == '_') then
                    pos = pos + 1
                    col_num = col_num + 1
                    do while (pos <= len(source))
                        c = source(pos:pos)
                        if (.not. ((c >= 'a' .and. c <= 'z') .or. &
                                   (c >= 'A' .and. c <= 'Z') .or. &
                                   (c >= '0' .and. c <= '9') .or. c == '_')) exit
                        pos = pos + 1
                        col_num = col_num + 1
                    end do
                    token_text = source(start_pos:pos - 1)
                end if
            end if
        end if

        ! Create token - check if its a logical constant or logical operator
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            if (is_logical_constant(token_text)) then
                tokens(token_count)%kind = TK_KEYWORD
            else
                tokens(token_count)%kind = TK_OPERATOR
            end if
            tokens(token_count)%text = token_text
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_logical_token

    ! Safe scanning functions
    subroutine scan_number_safe(source, pos, line_num, col_num, tokens, token_count, &
            scan_result)
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

    subroutine scan_identifier_safe(source, pos, line_num, col_num, tokens, &
            token_count, &
            scan_result)
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

    subroutine scan_comment_safe(source, pos, line_num, col_num, tokens, token_count, &
            scan_result)
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

    subroutine scan_operator_safe(source, pos, line_num, col_num, tokens, &
            token_count, &
            scan_result)
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

    subroutine scan_logical_token_safe(source, pos, line_num, col_num, tokens, &
            token_count, scan_result)
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

    ! Locate the first unterminated character constant in a source text.
    !
    ! A character context that is not closed before the end of its line is only
    ! legal when the line is continued and the next line resumes the constant
    ! with a leading ampersand (F2018 6.3.2.4). Otherwise the constant runs off
    ! the end of the line and off the end of the program unit. Returns the line
    ! and column of the opening delimiter.
    function find_unterminated_character_constant(source, line_num, col_num) &
            result(found)
        character(len=*), intent(in) :: source
        integer, intent(out) :: line_num, col_num
        logical :: found
        integer :: source_len, line_start, line_end, current_line, open_col
        logical :: quote_open

        found = .false.
        line_num = 0
        col_num = 0
        ! The scan understands free-form lines only.  Fixed-form sources split
        ! character constants across a column-6 continuation, which this scan
        ! would misread as an unterminated constant.
        if (source_is_fixed_form(source)) return
        source_len = len(source)
        line_start = 1
        current_line = 1

        do while (line_start <= source_len)
            line_end = line_start - 1
            do while (line_end < source_len)
                if (source(line_end + 1:line_end + 1) == char(10)) exit
                if (source(line_end + 1:line_end + 1) == char(13)) exit
                line_end = line_end + 1
            end do

            call scan_line_quotes(source(line_start:line_end), quote_open, open_col)
            if (quote_open) then
                ! A trailing ampersand continues the character constant on the
                ! next line.  The leading ampersand of the continuation line is
                ! optional in practice (compilers only warn when it is absent),
                ! so a continued line is never reported here.
                if (line_is_continued(source(line_start:line_end))) return
                found = .true.
                line_num = current_line
                col_num = open_col
                return
            end if

            if (line_end >= source_len) return
            line_start = line_end + 2
            if (source(line_end + 1:line_end + 1) == char(13)) then
                if (line_start <= source_len) then
                    if (source(line_start:line_start) == char(10)) then
                        line_start = line_start + 1
                    end if
                end if
            end if
            current_line = current_line + 1
        end do
    end function find_unterminated_character_constant

    ! Whether a line ends inside a character context, and where that context
    ! was opened. A comment delimiter outside a character context ends the line.
    subroutine scan_line_quotes(line, quote_open, open_col)
        character(len=*), intent(in) :: line
        logical, intent(out) :: quote_open
        integer, intent(out) :: open_col
        character :: c, quote_char
        integer :: i

        quote_open = .false.
        open_col = 0
        quote_char = ' '
        i = 1
        do while (i <= len(line))
            c = line(i:i)
            if (quote_open) then
                if (c == quote_char) then
                    if (i < len(line)) then
                        if (line(i + 1:i + 1) == quote_char) then
                            i = i + 2
                            cycle
                        end if
                    end if
                    quote_open = .false.
                end if
            else
                if (c == '!') return
                if (c == "'" .or. c == '"') then
                    quote_open = .true.
                    quote_char = c
                    open_col = i
                end if
            end if
            i = i + 1
        end do
    end subroutine scan_line_quotes

    ! Whether a line ends with a continuation ampersand.
    function line_is_continued(line) result(is_continued)
        character(len=*), intent(in) :: line
        logical :: is_continued
        integer :: trimmed_len

        is_continued = .false.
        trimmed_len = len_trim(line)
        if (trimmed_len == 0) return
        is_continued = line(trimmed_len:trimmed_len) == '&'
    end function line_is_continued

    ! Whether the source is fixed form.  Two independent signals are accepted:
    ! a column-6 continuation line, or a column-1 comment marker together with
    ! a statement indented to column 7.  Both are impossible in free form.
    function source_is_fixed_form(source) result(is_fixed)
        character(len=*), intent(in) :: source
        logical :: is_fixed
        logical :: saw_comment_marker, saw_column_seven_statement
        integer :: pos, line_start, line_end, source_len, i
        character :: c

        is_fixed = .false.
        saw_comment_marker = .false.
        saw_column_seven_statement = .false.
        source_len = len(source)
        line_start = 1

        do while (line_start <= source_len)
            line_end = line_start - 1
            do while (line_end < source_len)
                c = source(line_end + 1:line_end + 1)
                if (c == char(10)) exit
                if (c == char(13)) exit
                line_end = line_end + 1
            end do

            if (line_end - line_start + 1 >= 1) then
                c = source(line_start:line_start)
                if (c == 'C' .or. c == 'c' .or. c == '*') then
                    saw_comment_marker = .true.
                end if
            end if

            if (line_end - line_start + 1 >= 7) then
                if (source(line_start:line_start + 5) == '      ') then
                    saw_column_seven_statement = .true.
                end if
            end if

            if (saw_comment_marker .and. saw_column_seven_statement) then
                is_fixed = .true.
                return
            end if

            if (line_end - line_start + 1 >= 6) then
                is_fixed = .true.
                do i = 0, 4
                    pos = line_start + i
                    c = source(pos:pos)
                    if (c == ' ') cycle
                    if (c == char(9)) cycle
                    if (c >= '0' .and. c <= '9') cycle
                    is_fixed = .false.
                    exit
                end do
                if (is_fixed) then
                    c = source(line_start + 5:line_start + 5)
                    is_fixed = c /= ' ' .and. c /= '0' .and. c /= char(9)
                end if
                if (is_fixed) return
            end if

            if (line_end >= source_len) return
            line_start = line_end + 2
            if (source(line_end + 1:line_end + 1) == char(13)) then
                if (line_start <= source_len) then
                    if (source(line_start:line_start) == char(10)) then
                        line_start = line_start + 1
                    end if
                end if
            end if
        end do
    end function source_is_fixed_form


    ! Helper function to check if a word is a keyword
    function is_keyword(word) result(keyword)
        character(len=*), intent(in) :: word
        logical :: keyword
        character(len=:), allocatable :: lower_word

        lower_word = to_lower(word)

        select case (trim(lower_word))
        case ('program', 'end', 'function', 'subroutine', 'if', &
                'then', 'else', &
                'go', 'goto', 'cycle', 'exit', 'stop', 'pause', 'return', &
                'entry', 'error', &
                'continue', 'nullify', 'do', 'while', 'concurrent', 'for', &
                'integer', &
                'real', 'logical', 'character', 'complex', 'double', &
                'precision', &
                'implicit', 'none', 'parameter', 'dimension', 'allocatable', &
                'intent', 'use', 'module', 'contains', 'public', 'private', &
                'namelist', 'data', 'type', 'class', 'extends', 'abstract', &
                'procedure', 'interface', 'import', 'include', 'generic', &
                'operator', &
                'assignment', 'print', 'read', 'write', 'open', 'close', &
                'inquire', &
                'backspace', 'rewind', &
                'call', 'format', 'allocate', 'deallocate', 'select', 'case', &
                'default', &
                'where', 'associate', 'forall', 'block', 'enum', 'file', &
                'submodule', 'rank', 'elseif', 'elsewhere', 'blockdata', &
                'doubleprecision', 'doublecomplex', 'selectcase', 'equivalence', &
                'common', 'endif', 'enddo', 'endwhere', 'endforall', &
                'endassociate', 'endblock', 'endblockdata', 'endenum', &
                'endfile', &
                'endfunction', 'endinterface', 'endmodule', 'endprogram', &
                'endselect', 'endsubmodule', 'endsubroutine', 'endtype', &
                'template', 'endtemplate', 'instantiate', &
                'trait', 'endtrait', 'requirement', 'endrequirement', &
                'implements', 'endimplements', &
                'elemental', 'pure', 'impure', 'recursive', 'nonrecursive', &
                'non_recursive', 'intrinsic', 'non_intrinsic', 'external')
            keyword = .true.
        case default
            keyword = .false.
        end select
    end function is_keyword

    ! Helper function to check if a logical token is a constant (.true./.false.)
    function is_logical_constant(token_text) result(is_constant)
        character(len=*), intent(in) :: token_text
        logical :: is_constant
        character(len=:), allocatable :: lower_text

        lower_text = to_lower(token_text)

        select case (trim(lower_text))
        case ('.true.', '.false.')
            is_constant = .true.
        case default
            is_constant = (index(trim(lower_text), '.true._') == 1 .or. &
                           index(trim(lower_text), '.false._') == 1)
        end select
    end function is_logical_constant

    ! True for characters that may appear in free-form source text outside of
    ! comments and character literals: printable ASCII plus tab/newline/CR.
    pure logical function is_legal_source_char(c) result(legal)
        character, intent(in) :: c
        integer :: code

        code = iachar(c)
        legal = .false.
        if (code >= 32 .and. code <= 126) legal = .true.
        if (code == 9) legal = .true.
        if (code == 10) legal = .true.
        if (code == 13) legal = .true.
    end function is_legal_source_char

    ! True for characters that may continue a Fortran name.
    pure logical function is_name_body_char(c) result(is_body)
        character, intent(in) :: c

        select case (c)
        case ('a':'z', 'A':'Z', '0':'9', '_')
            is_body = .true.
        case default
            is_body = .false.
        end select
    end function is_name_body_char

    ! True for characters that may legally precede a '%' part-reference or
    ! type-parameter inquiry separator.
    pure logical function is_percent_prefix_char(c) result(is_prefix)
        character, intent(in) :: c

        select case (c)
        case ('a':'z', 'A':'Z', '0':'9', '_', ')', ']')
            is_prefix = .true.
        case default
            is_prefix = .false.
        end select
    end function is_percent_prefix_char

end module lexer_scanners
