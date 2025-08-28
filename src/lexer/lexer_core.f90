module lexer_core
    ! Main lexer module that re-exports functionality from split modules
    use lexer_token_types
    use lexer_scanners
    use error_handling
    implicit none
    private

    ! Re-export all token types and constants
    public :: TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_OPERATOR, TK_KEYWORD
    public :: TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_UNKNOWN
    public :: TOKEN_WHITESPACE, TOKEN_COMMENT, TOKEN_NEWLINE
    
    ! Re-export types
    public :: trivia_token_t, token_t, lexer_options_t, tokenize_result_t, scan_result_t
    
    ! Re-export main functions
    public :: tokenize_safe, tokenize_core, tokenize_core_safe
    public :: token_type_name
    
    ! Re-export scanning functions
    public :: scan_number, scan_comment, scan_string, scan_identifier, scan_operator, scan_logical_token
    public :: scan_number_safe, scan_comment_safe, scan_string_safe, scan_identifier_safe
    public :: scan_operator_safe, scan_logical_token_safe
    
    ! Re-export utilities
    public :: to_lower, resize_tokens, resize_trivia_buffer

contains

    ! Main tokenization function with error handling
    function tokenize_safe(source) result(tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t) :: tokenize_res
        
        ! Initialize result
        tokenize_res%success = .true.
        tokenize_res%result = success_result()
        tokenize_res%token_count = 0
        
        ! Allocate initial token array
        allocate(tokenize_res%tokens(1000))
        
        ! Call core tokenization
        call tokenize_core_safe(source, tokenize_res)
        
        ! Resize to actual count
        if (tokenize_res%success .and. tokenize_res%token_count > 0) then
            call resize_tokens_to_count(tokenize_res%tokens, tokenize_res%token_count)
        end if
    end function tokenize_safe

    ! Core tokenization logic
    subroutine tokenize_core(source, tokens)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(tokenize_result_t) :: tokenize_res
        
        tokenize_res = tokenize_safe(source)
        
        if (tokenize_res%success) then
            allocate(tokens(tokenize_res%token_count))
            tokens = tokenize_res%tokens(1:tokenize_res%token_count)
        else
            allocate(tokens(0))
        end if
    end subroutine tokenize_core

    ! Safe tokenization with detailed error handling
    subroutine tokenize_core_safe(source, tokenize_res)
        character(len=*), intent(in) :: source
        type(tokenize_result_t), intent(inout) :: tokenize_res
        integer :: pos, line_num, col_num, source_len
        character :: c
        
        pos = 1
        line_num = 1
        col_num = 1
        source_len = len(source)
        tokenize_res%token_count = 0
        
        do while (pos <= source_len)
            c = source(pos:pos)
            
            select case (c)
            case (' ', char(9))  ! Space, tab
                call skip_whitespace(source, pos, line_num, col_num)
                
            case (char(10), char(13))  ! Newline
                call handle_newline(source, pos, line_num, col_num, &
                                   tokenize_res%tokens, tokenize_res%token_count)
                
            case ('!')  ! Comment
                call scan_comment(source, pos, line_num, col_num, &
                                 tokenize_res%tokens, tokenize_res%token_count)
                
            case ('''', '"')  ! String
                call scan_string(source, pos, line_num, col_num, &
                                tokenize_res%tokens, tokenize_res%token_count)
                
            case ('0':'9')  ! Number
                call scan_number(source, pos, line_num, col_num, &
                                tokenize_res%tokens, tokenize_res%token_count)
                
            case ('a':'z', 'A':'Z', '_')  ! Identifier
                call scan_identifier(source, pos, line_num, col_num, &
                                    tokenize_res%tokens, tokenize_res%token_count)
                
            case ('.')  ! Logical operator or number
                if (pos < source_len .and. source(pos+1:pos+1) >= '0' .and. source(pos+1:pos+1) <= '9') then
                    call scan_number(source, pos, line_num, col_num, &
                                    tokenize_res%tokens, tokenize_res%token_count)
                else
                    call scan_logical_token(source, pos, line_num, col_num, &
                                          tokenize_res%tokens, tokenize_res%token_count)
                end if
                
            case default  ! Operator or unknown
                if (is_operator_char(c)) then
                    call scan_operator(source, pos, line_num, col_num, &
                                      tokenize_res%tokens, tokenize_res%token_count)
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

    ! Handle newline characters
    subroutine handle_newline(source, pos, line_num, col_num, tokens, token_count)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos, line_num, col_num, token_count
        type(token_t), intent(inout) :: tokens(:)
        integer :: start_pos, start_col
        
        start_pos = pos
        start_col = col_num
        
        ! Handle CRLF or LF
        if (source(pos:pos) == char(13) .and. pos < len(source) .and. source(pos+1:pos+1) == char(10)) then
            pos = pos + 2
        else
            pos = pos + 1
        end if
        
        line_num = line_num + 1
        col_num = 1
        
        ! Create newline token
        if (token_count < size(tokens)) then
            token_count = token_count + 1
            tokens(token_count)%kind = TK_NEWLINE
            tokens(token_count)%text = source(start_pos:pos-1)
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
              '{', '}', ',', ';', ':', '%', '&')
            is_op = .true.
        case default
            is_op = .false.
        end select
    end function is_operator_char

    ! Resize tokens array
    subroutine resize_tokens(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), allocatable :: temp(:)
        integer :: old_size, new_size
        
        old_size = size(tokens)
        new_size = old_size * 2
        
        allocate(temp(new_size))
        temp(1:old_size) = tokens
        call move_alloc(temp, tokens)
    end subroutine resize_tokens

    ! Resize tokens array to specific count
    subroutine resize_tokens_to_count(tokens, count)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer, intent(in) :: count
        type(token_t), allocatable :: temp(:)
        
        if (count <= 0) then
            deallocate(tokens)
            allocate(tokens(0))
            return
        end if
        
        allocate(temp(count))
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
        
        allocate(temp(new_size))
        temp(1:old_size) = trivia
        call move_alloc(temp, trivia)
    end subroutine resize_trivia_buffer

    ! Re-export to_lower from scanners module
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

end module lexer_core