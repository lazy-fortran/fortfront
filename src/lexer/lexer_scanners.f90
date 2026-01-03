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
                tokens(token_count)%text = source(start_pos:pos - 1) // quote_char
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
                if (source(pos:pos) == ':') then
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

        ! Get the token text
        token_text = source(start_pos:pos - 1)

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
              'non_recursive', 'intrinsic', 'non_intrinsic')
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
            is_constant = .false.
        end select
    end function is_logical_constant

    ! Helper function to convert string to lowercase

end module lexer_scanners
