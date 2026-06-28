module lexer_core
    ! Main lexer module that re-exports functionality from split modules
    use lexer_token_types
    use lexer_scanners
    use error_handling
    use string_utils_mod, only: to_lower
    implicit none
    private

    ! Re-export all token types and constants
    public :: TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_OPERATOR, TK_KEYWORD
    public :: TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_UNKNOWN
    public :: TOKEN_WHITESPACE, TOKEN_COMMENT, TOKEN_NEWLINE

    ! Re-export types
    public :: trivia_token_t, token_t, lexer_options_t, tokenize_result_t, &
        scan_result_t

    ! Re-export main functions
    public :: tokenize_safe, tokenize_core, tokenize_core_safe
    public :: tokenize_safe_with_trivia, tokenize_core_with_trivia
    public :: normalize_line_endings
    public :: token_type_name

    ! Re-export scanning functions
    public :: scan_number, scan_comment, scan_string, scan_identifier, scan_operator, &
        scan_logical_token
    public :: scan_number_safe, scan_comment_safe, scan_string_safe, &
        scan_identifier_safe
    public :: scan_operator_safe, scan_logical_token_safe

    ! Re-export utilities
    public :: to_lower, resize_tokens, resize_trivia_buffer

contains

    pure function normalize_line_endings(source) result(normalized)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: normalized
        character(len=:), allocatable :: buffer
        integer :: src_len, i, write_pos
        character :: c

        src_len = len(source)
        if (src_len == 0) then
            allocate (character(len=0) :: normalized)
            return
        end if

        allocate (character(len=src_len) :: buffer)
        write_pos = 0

        do i = 1, src_len
            c = source(i:i)
            select case (c)
            case (char(13))
                if (i < src_len) then
                    if (source(i + 1:i + 1) == char(10)) cycle
                end if
                write_pos = write_pos + 1
                buffer(write_pos:write_pos) = char(10)
            case default
                write_pos = write_pos + 1
                buffer(write_pos:write_pos) = c
            end select
        end do

        if (write_pos == 0) then
            allocate (character(len=0) :: normalized)
        else
            allocate (character(len=write_pos) :: normalized)
            normalized = buffer(1:write_pos)
        end if
    end function normalize_line_endings

    ! Main tokenization function with error handling
    function tokenize_safe(source) result(tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t) :: tokenize_res

        ! Initialize result
        tokenize_res%success = .true.
        tokenize_res%result = success_result()
        tokenize_res%token_count = 0

        ! Allocate initial token array
        allocate (tokenize_res%tokens(1000))

        ! Call core tokenization
        call tokenize_core_safe(source, tokenize_res)

        ! Resize to actual count
        if (tokenize_res%success .and. tokenize_res%token_count > 0) then
            call resize_tokens_to_count(tokenize_res%tokens, tokenize_res%token_count)
        end if
    end function tokenize_safe

    ! Tokenization that preserves trivia and attaches it to tokens.
    function tokenize_safe_with_trivia(source) result(tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t) :: tokenize_res

        tokenize_res%success = .true.
        tokenize_res%result = success_result()
        tokenize_res%token_count = 0

        allocate (tokenize_res%tokens(1000))

        call tokenize_core_safe_with_trivia(source, tokenize_res)

        if (tokenize_res%success .and. tokenize_res%token_count > 0) then
            call resize_tokens_to_count(tokenize_res%tokens, tokenize_res%token_count)
        end if
    end function tokenize_safe_with_trivia

    ! Core tokenization logic
    subroutine tokenize_core(source, tokens)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(tokenize_result_t) :: tokenize_res

        tokenize_res = tokenize_safe(source)

        if (tokenize_res%success) then
            allocate (tokens(tokenize_res%token_count))
            tokens = tokenize_res%tokens(1:tokenize_res%token_count)
        else
            allocate (tokens(0))
        end if
    end subroutine tokenize_core

    ! Core tokenization that preserves trivia and attaches it to tokens.
    subroutine tokenize_core_with_trivia(source, tokens)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(tokenize_result_t) :: tokenize_res

        tokenize_res = tokenize_safe_with_trivia(source)

        if (tokenize_res%success) then
            allocate (tokens(tokenize_res%token_count))
            tokens = tokenize_res%tokens(1:tokenize_res%token_count)
        else
            allocate (tokens(0))
        end if
    end subroutine tokenize_core_with_trivia

    ! Safe tokenization with detailed error handling
    subroutine tokenize_core_safe(source, tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t), intent(inout) :: tokenize_res
        character(len=:), allocatable :: src
        integer :: pos, line_num, col_num, source_len
        character :: c

        src = normalize_line_endings(source)

        pos = 1
        line_num = 1
        col_num = 1
        source_len = len(src)
        tokenize_res%token_count = 0

        do while (pos <= source_len)
            c = src(pos:pos)

            select case (c)
            case (' ', char(9)) ! Space, tab
                call skip_whitespace(src, pos, line_num, col_num)

            case (char(10), char(13)) ! Newline
                call handle_newline(src, pos, line_num, col_num, &
                    tokenize_res%tokens, tokenize_res%token_count)

            case ('!', '#') ! Comment or preprocessor directive
                call scan_comment(src, pos, line_num, col_num, &
                    tokenize_res%tokens, tokenize_res%token_count)

            case ('''', '"') ! String
                call scan_string(src, pos, line_num, col_num, &
                    tokenize_res%tokens, tokenize_res%token_count)

            case ('0':'9') ! Number
                call scan_number(src, pos, line_num, col_num, &
                    tokenize_res%tokens, tokenize_res%token_count)

            case ('a':'z', 'A':'Z', '_') ! Identifier
                call scan_identifier(src, pos, line_num, col_num, &
                    tokenize_res%tokens, &
                    tokenize_res%token_count)

            case ('.')
                ! Logical token, decimal number, assumed-rank, or separator
                call scan_dot_token(src, pos, line_num, col_num, &
                    tokenize_res%tokens, &
                    tokenize_res%token_count)

            case default ! Operator or unknown
                if (is_operator_char(c)) then
                    call scan_operator(src, pos, line_num, col_num, &
                        tokenize_res%tokens, &
                        tokenize_res%token_count)
                else
                    ! Unknown character - skip
                    pos = pos + 1
                    col_num = col_num + 1
                end if
            end select

            ! Check for buffer overflow
            if (tokenize_res%token_count >= size(tokenize_res%tokens)) then
                call resize_tokens(tokenize_res%tokens)
            end if
        end do

        ! Add EOF token
        if (tokenize_res%token_count < size(tokenize_res%tokens)) then
            tokenize_res%token_count = tokenize_res%token_count + 1
            tokenize_res%tokens(tokenize_res%token_count)%kind = TK_EOF
            tokenize_res%tokens(tokenize_res%token_count)%text = ""
            tokenize_res%tokens(tokenize_res%token_count)%line = line_num
            tokenize_res%tokens(tokenize_res%token_count)%column = col_num
        end if

        tokenize_res%success = .true.
    end subroutine tokenize_core_safe

    subroutine tokenize_core_safe_with_trivia(source, tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t), intent(inout) :: tokenize_res
        character(len=:), allocatable :: src
        type(trivia_token_t), allocatable :: pending_trivia(:)
        integer :: pos, line_num, col_num, source_len

        src = normalize_line_endings(source)

        pos = 1
        line_num = 1
        col_num = 1
        source_len = len(src)
        tokenize_res%token_count = 0

        do while (pos <= source_len)
            call process_token_with_trivia(src, pos, line_num, col_num, &
                tokenize_res%tokens, &
                tokenize_res%token_count, &
                pending_trivia)

            if (tokenize_res%token_count >= size(tokenize_res%tokens)) then
                call resize_tokens(tokenize_res%tokens)
            end if
        end do

        call attach_final_trivia(tokenize_res%tokens, tokenize_res%token_count, &
            pending_trivia)

        if (tokenize_res%token_count < size(tokenize_res%tokens)) then
            tokenize_res%token_count = tokenize_res%token_count + 1
            tokenize_res%tokens(tokenize_res%token_count)%kind = TK_EOF
            tokenize_res%tokens(tokenize_res%token_count)%text = ""
            tokenize_res%tokens(tokenize_res%token_count)%line = line_num
            tokenize_res%tokens(tokenize_res%token_count)%column = col_num
        end if

        tokenize_res%success = .true.
    end subroutine tokenize_core_safe_with_trivia

    subroutine process_token_with_trivia(source_text, pos, line_num, col_num, &
            tokens, token_count, pending_trivia)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        type(trivia_token_t), allocatable, intent(inout) :: pending_trivia(:)

        integer :: previous_count
        character :: c

        c = source_text(pos:pos)

        select case (c)
        case (' ', char(9)) ! Space, tab
            call collect_whitespace_trivia(source_text, pos, &
                line_num, col_num, &
                pending_trivia)
        case (char(10), char(13)) ! Newline
            call collect_newline_trivia(source_text, pos, line_num, col_num, &
                pending_trivia)
        case ('!', '#') ! Comment or preprocessor directive
            call collect_comment_trivia(source_text, pos, line_num, col_num, &
                pending_trivia)
        case ('''', '"') ! String
            previous_count = token_count
            call scan_string(source_text, pos, line_num, col_num, tokens, &
                token_count)
            call attach_pending_trivia(tokens, previous_count, token_count, &
                pending_trivia)
        case ('0':'9') ! Number
            previous_count = token_count
            call scan_number(source_text, pos, line_num, col_num, tokens, &
                token_count)
            call attach_pending_trivia(tokens, previous_count, token_count, &
                pending_trivia)
        case ('a':'z', 'A':'Z', '_') ! Identifier
            previous_count = token_count
            call scan_identifier(source_text, pos, line_num, col_num, tokens, &
                token_count)
            call attach_pending_trivia(tokens, previous_count, token_count, &
                pending_trivia)
        case ('.') ! Logical token, decimal number, assumed-rank, or separator
            previous_count = token_count
            call scan_dot_token(source_text, pos, line_num, col_num, tokens, &
                token_count)
            call attach_pending_trivia(tokens, previous_count, token_count, &
                pending_trivia)
        case default ! Operator or unknown
            if (is_operator_char(c)) then
                previous_count = token_count
                call scan_operator(source_text, pos, line_num, &
                    col_num, tokens, &
                    token_count)
                call attach_pending_trivia(tokens, previous_count, &
                    token_count, &
                    pending_trivia)
            else
                pos = pos + 1
                col_num = col_num + 1
            end if
        end select
    end subroutine process_token_with_trivia

    subroutine scan_dot_token(source, pos, line_num, col_num, tokens, &
            token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)

        integer :: source_len
        logical :: prev_is_digit
        character :: next_char
        integer :: probe_pos
        logical :: looks_like_dot_word_dot

        source_len = len(source)
        prev_is_digit = .false.
        looks_like_dot_word_dot = .false.

        if (pos < source_len) then
            next_char = source(pos + 1:pos + 1)
            if (next_char == '.') then
                call scan_assumed_rank_operator(source, pos, &
                    line_num, col_num, &
                    tokens, token_count)
            else if (next_char >= '0' .and. next_char <= '9') then
                if (pos > 1) then
                    prev_is_digit = (source(pos - 1:pos - 1) >= '0' .and. &
                        source(pos - 1:pos - 1) <= '9')
                end if
                if (prev_is_digit) then
                    call scan_operator(source, pos, line_num, col_num, tokens, &
                        token_count)
                else
                    call scan_number(source, pos, line_num, col_num, tokens, &
                        token_count)
                end if
            else if ((next_char >= 'a' .and. next_char <= 'z') .or. &
                    (next_char >= 'A' .and. next_char <= 'Z') .or. &
                    next_char == '_') then
                ! Disambiguation:
                ! - .and. / .or. / .op. / .true. etc: dot + identifier + dot
                ! - LFortran member access: identifier.identifier (no trailing dot)
                probe_pos = pos + 1
                do while (probe_pos <= source_len)
                    next_char = source(probe_pos:probe_pos)
                    if ((next_char >= 'a' .and. next_char <= 'z') .or. &
                        (next_char >= 'A' .and. next_char <= 'Z') .or. &
                        (next_char >= '0' .and. next_char <= '9') .or. &
                        next_char == '_') then
                        probe_pos = probe_pos + 1
                        cycle
                    end if
                    exit
                end do
                if (probe_pos <= source_len) then
                    looks_like_dot_word_dot = (source(probe_pos:probe_pos) == '.')
                end if

                if (looks_like_dot_word_dot) then
                    call scan_logical_token(source, pos, line_num, &
                        col_num, tokens, &
                        token_count)
                else
                    call scan_operator(source, pos, line_num, col_num, tokens, &
                        token_count)
                end if
            else
                call scan_logical_token(source, pos, line_num, &
                    col_num, tokens, &
                    token_count)
            end if
        else
            call scan_operator(source, pos, line_num, col_num, tokens, &
                token_count)
        end if
    end subroutine scan_dot_token

    subroutine append_trivia(buffer, kind, text, line, column)
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)
        integer, intent(in) :: kind, line, column
        character(len=*), intent(in) :: text
        type(trivia_token_t), allocatable :: tmp(:)
        integer :: current_size

        if (len(text) <= 0) return

        if (allocated(buffer)) then
            current_size = size(buffer)
            allocate (tmp(current_size + 1))
            if (current_size > 0) tmp(1:current_size) = buffer
        else
            current_size = 0
            allocate (tmp(1))
        end if

        tmp(current_size + 1)%kind = kind
        tmp(current_size + 1)%text = text
        tmp(current_size + 1)%line = line
        tmp(current_size + 1)%column = column

        call move_alloc(tmp, buffer)
    end subroutine append_trivia

    subroutine clear_trivia(buffer)
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)
        if (allocated(buffer)) deallocate (buffer)
    end subroutine clear_trivia

    subroutine collect_whitespace_trivia(source_text, pos, line, column, buffer)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos, line, column
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)
        integer :: start_pos, start_col
        character :: ch

        start_pos = pos
        start_col = column

        do while (pos <= len(source_text))
            ch = source_text(pos:pos)
            if (ch == ' ' .or. ch == char(9)) then
                pos = pos + 1
                column = column + 1
            else
                exit
            end if
        end do

        call append_trivia(buffer, TK_WHITESPACE, &
            source_text(start_pos:pos - 1), &
            line, start_col)
    end subroutine collect_whitespace_trivia

    subroutine collect_newline_trivia(source_text, pos, line, column, buffer)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos, line, column
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)
        integer :: start_line, start_col

        start_line = line
        start_col = column

        call append_trivia(buffer, TK_NEWLINE, new_line('A'), &
            start_line, start_col)

        call skip_newline(source_text, pos, line, column)
    end subroutine collect_newline_trivia

    subroutine collect_comment_trivia(source_text, pos, line, column, buffer)
        character(len=*), intent(in) :: source_text
        integer, intent(inout) :: pos, line, column
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)
        integer :: start_pos, start_col, end_pos
        character :: ch

        start_pos = pos
        start_col = column

        end_pos = start_pos
        do while (end_pos <= len(source_text))
            ch = source_text(end_pos:end_pos)
            if (ch == char(10) .or. ch == char(13)) exit
            end_pos = end_pos + 1
        end do

        if (end_pos > start_pos) then
            call append_trivia(buffer, TK_COMMENT, &
                source_text(start_pos:end_pos - 1), &
                line, start_col)
        end if

        column = column + (end_pos - start_pos)
        pos = end_pos
    end subroutine collect_comment_trivia

    subroutine attach_pending_trivia(tokens, previous_count, &
            current_count, buffer)
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(in) :: previous_count
        integer, intent(in) :: current_count
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)

        if (.not. allocated(buffer)) return
        if (current_count <= 0) then
            call clear_trivia(buffer)
            return
        end if

        if (previous_count > 0) then
            if (allocated(tokens(previous_count)%trailing_trivia)) then
                deallocate (tokens(previous_count)%trailing_trivia)
            end if
            tokens(previous_count)%trailing_trivia = buffer
        end if

        if (allocated(tokens(current_count)%leading_trivia)) then
            deallocate (tokens(current_count)%leading_trivia)
        end if
        tokens(current_count)%leading_trivia = buffer

        call clear_trivia(buffer)
    end subroutine attach_pending_trivia

    subroutine attach_final_trivia(tokens, token_count, buffer)
        type(token_t), intent(inout) :: tokens(:)
        integer, intent(in) :: token_count
        type(trivia_token_t), allocatable, intent(inout) :: buffer(:)

        if (.not. allocated(buffer)) return
        if (token_count <= 0) then
            call clear_trivia(buffer)
            return
        end if

        if (allocated(tokens(token_count)%trailing_trivia)) then
            deallocate (tokens(token_count)%trailing_trivia)
        end if
        tokens(token_count)%trailing_trivia = buffer

        call clear_trivia(buffer)
    end subroutine attach_final_trivia

    ! Skip whitespace characters
    subroutine skip_whitespace(source, pos, line_num, col_num)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num
        character :: c

        do while (pos <= len(source))
            c = source(pos:pos)
            if (c == ' ' .or. c == char(9)) then
                pos = pos + 1
                col_num = col_num + 1
            else
                exit
            end if
        end do
    end subroutine skip_whitespace

    ! Skip newline characters (update line/column tracking without creating tokens)
    subroutine skip_newline(source, pos, line_num, col_num)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num

        ! Handle CRLF or LF - with proper bounds checking
        if (pos <= len(source)) then
            if (source(pos:pos) == char(13) .and. pos + 1 <= len(source)) then
                if (source(pos + 1:pos + 1) == char(10)) then
                    pos = pos + 2
                else
                    pos = pos + 1
                end if
            else
                pos = pos + 1
            end if
        else
            pos = pos + 1
        end if

        line_num = line_num + 1
        col_num = 1
    end subroutine skip_newline

    ! Handle newline characters
    subroutine handle_newline(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col

        start_pos = pos
        start_col = col_num

        ! Handle CRLF or LF - with proper bounds checking
        if (pos <= len(source)) then
            if (source(pos:pos) == char(13) .and. pos + 1 <= len(source)) then
                if (source(pos + 1:pos + 1) == char(10)) then
                    pos = pos + 2
                else
                    pos = pos + 1
                end if
            else
                pos = pos + 1
            end if
        else
            pos = pos + 1
        end if

        line_num = line_num + 1
        col_num = 1

        ! Create newline token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_NEWLINE
            tokens(token_count)%text = source(start_pos:pos - 1)
            tokens(token_count)%line = line_num - 1
            tokens(token_count)%column = start_col
        end if
    end subroutine handle_newline

    ! Check if character is an operator
    function is_operator_char(c) result(is_op)
        character, intent(in) :: c
        logical :: is_op

        select case (c)
        case ('+', '-', '*', '/', '=', '<', '>', '(', ')', '[', ']', &
                '{', '}', ',', ';', ':', '%', '&', '^')
            is_op = .true.
        case default
            is_op = .false.
        end select
    end function is_operator_char

    ! Resize tokens array
    subroutine resize_tokens(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), allocatable, target :: temp(:)
        integer :: old_size, new_size

        old_size = size(tokens)
        new_size = old_size * 2

        allocate (temp(new_size))
        temp(1:old_size) = tokens
        call move_alloc(temp, tokens)
    end subroutine resize_tokens

    ! Resize tokens array to specific count
    subroutine resize_tokens_to_count(tokens, count)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer, intent(in) :: count
        type(token_t), allocatable, target :: temp(:)

        if (count <= 0) then
            deallocate (tokens)
            allocate (tokens(0))
            return
        end if

        allocate (temp(count))
        temp(1:min(count, size(tokens))) = tokens(1:min(count, size(tokens)))
        call move_alloc(temp, tokens)
    end subroutine resize_tokens_to_count

    ! Resize trivia buffer
    subroutine resize_trivia_buffer(trivia)
        type(trivia_token_t), allocatable, intent(inout) :: trivia(:)
        type(trivia_token_t), allocatable :: temp(:)
        integer :: old_size, new_size

        old_size = size(trivia)
        new_size = old_size * 2

        allocate (temp(new_size))
        temp(1:old_size) = trivia
        call move_alloc(temp, trivia)
    end subroutine resize_trivia_buffer

    ! Scan assumed-rank operator (..) - Fortran 2018 feature for SELECT RANK
    subroutine scan_assumed_rank_operator(source, pos, line_num, col_num, &
            tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col

        start_pos = pos
        start_col = col_num

        ! Consume both dots (..)
        pos = pos + 2
        col_num = col_num + 2

        ! Create operator token for assumed-rank
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_OPERATOR
            tokens(token_count)%text = ".."
            tokens(token_count)%line = line_num
            tokens(token_count)%column = start_col
        end if
    end subroutine scan_assumed_rank_operator

end module lexer_core
