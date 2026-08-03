
    ! Legacy format function for backward compatibility
    function format_syntax_error(message, line, column, source_lines, suggestion) result(formatted)
        character(len=*), intent(in) :: message
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: source_lines(:)
        character(len=*), intent(in) :: suggestion
        character(len=:), allocatable :: formatted

        ! Just delegate to enhanced version with default error type
        if (len(suggestion) > 0) then
            formatted = format_enhanced_error(message, line, column, source_lines, suggestion, "SYNTAX_ERROR")
        else
            formatted = format_enhanced_error(message, line, column, source_lines, "Check syntax", "SYNTAX_ERROR")
        end if
    end function format_syntax_error

    ! Split source code into lines with dynamic allocation to prevent buffer overflow
    subroutine split_into_lines(source, lines)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: lines(:)

        integer :: i, line_count, start_pos, current_pos, max_line_len
        character(len=:), allocatable :: temp_lines(:)
        integer :: line_length

        ! Handle empty source
        if (len(source) == 0) then
            allocate (character(len=1) :: lines(1))
            lines(1) = ""
            return
        end if

        ! First pass: count lines to determine array size
        line_count = 1  ! At least one line even if no newlines
        do current_pos = 1, len(source)
            if (source(current_pos:current_pos) == new_line('A')) then
                line_count = line_count + 1
            end if
        end do

        ! Second pass: determine maximum line length
        max_line_len = 0
        start_pos = 1
        do current_pos = 1, len(source)
            if (source(current_pos:current_pos) == new_line('A')) then
                line_length = current_pos - start_pos
                max_line_len = max(max_line_len, line_length)
                start_pos = current_pos + 1
            end if
        end do

        ! Check the last line if it doesn't end with newline
        if (start_pos <= len(source)) then
            line_length = len(source) - start_pos + 1
            max_line_len = max(max_line_len, line_length)
        end if

        ! Ensure minimum length
        max_line_len = max(max_line_len, 1)

        ! Dynamically allocate storage for all lines
        allocate (character(len=max_line_len) :: temp_lines(line_count))

        ! Third pass: extract lines
        i = 1
        start_pos = 1
        do current_pos = 1, len(source)
            if (source(current_pos:current_pos) == new_line('A')) then
                if (current_pos > start_pos) then
                    temp_lines(i) = source(start_pos:current_pos - 1)
                else
                    temp_lines(i) = ""
                end if
                i = i + 1
                start_pos = current_pos + 1
            end if
        end do

        ! Add the last line if it doesn't end with newline
        if (start_pos <= len(source) .and. i <= line_count) then
            temp_lines(i) = source(start_pos:len(source))
        end if

        ! Allocate output array and copy lines
        allocate (character(len=max_line_len) :: lines(line_count))
        do i = 1, line_count
            lines(i) = temp_lines(i)
        end do

    end subroutine split_into_lines

    ! Check if tokens represent likely valid Fortran code
    logical function is_likely_valid_fortran(tokens) result(is_valid)
        type(token_t), intent(in) :: tokens(:)
        integer :: i, identifier_count, number_count, operator_count, unknown_count
        logical :: has_assignment, has_function_call

        identifier_count = 0
        number_count = 0
        operator_count = 0
        unknown_count = 0
        has_assignment = .false.
        has_function_call = .false.

        do i = 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_IDENTIFIER)
                identifier_count = identifier_count + 1
                ! Check for function call pattern
                if (i < size(tokens) .and. tokens(i + 1)%text == "(") then
                    has_function_call = .true.
                end if
            case (TK_NUMBER)
                number_count = number_count + 1
            case (TK_OPERATOR)
                operator_count = operator_count + 1
                if (tokens(i)%text == "=") then
                    has_assignment = .true.
                end if
            case (TK_UNKNOWN)
                unknown_count = unknown_count + 1
            end select
        end do

        ! Consider valid if has balanced structure and no unknown tokens
        ! Accept single identifiers as valid expressions (for lazy Fortran)
        ! Accept numerical expressions (e.g., "2 + 3", "42 * 3.14")
        is_valid = ((identifier_count > 0) .or. (number_count > 0 .and. operator_count > 0)) .and. &
                   (has_assignment .or. has_function_call .or. (operator_count > 0) .or. &
                    (identifier_count == 1 .and. number_count == 0 .and. operator_count == 0) .or. &
                    (number_count > 0 .and. operator_count > 0)) .and. &
                   (unknown_count == 0)
    end function is_likely_valid_fortran

    ! Check if tokens have any recognizable Fortran patterns
    logical function has_any_fortran_patterns(tokens) result(has_patterns)
        type(token_t), intent(in) :: tokens(:)
        integer :: i, identifier_count, number_count, special_char_count, unknown_count
        logical :: has_operators, has_keywords

        identifier_count = 0
        number_count = 0
        special_char_count = 0
        unknown_count = 0
        has_operators = .false.
        has_keywords = .false.

        do i = 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_IDENTIFIER)
                identifier_count = identifier_count + 1
            case (TK_NUMBER)
                number_count = number_count + 1
            case (TK_KEYWORD)
                has_keywords = .true.
            case (TK_OPERATOR)
                has_operators = .true.
            case (TK_UNKNOWN)
                unknown_count = unknown_count + 1
                ! Count characters that are unlikely in Fortran
                if (len(tokens(i)%text) > 0) then
                    if (index(tokens(i)%text, '@') > 0 .or. &
                        index(tokens(i)%text, '#') > 0 .or. &
                        index(tokens(i)%text, '$') > 0 .or. &
                        index(tokens(i)%text, '%') > 0 .or. &
                        index(tokens(i)%text, '***') > 0) then
                        special_char_count = special_char_count + 1
                    end if
                end if
            end select
        end do

        ! Reject input if too many unknown/special characters relative to meaningful content
        if (unknown_count > 0 .and. special_char_count > 0) then
            has_patterns = .false.
            ! Accept if has keywords or structured patterns (operators with identifiers)
        else if (has_keywords .or. (has_operators .and. identifier_count > 0)) then
            has_patterns = .true.
            ! Require more structure for acceptance
        else
            has_patterns = (identifier_count >= 2 .and. number_count > 0) .and. &
                           (special_char_count + unknown_count == 0)
        end if
    end function has_any_fortran_patterns

    ! Check if tokens represent a likely Fortran expression
    logical function is_likely_fortran_expression(tokens) result(is_expression)
        type(token_t), intent(in) :: tokens(:)
        integer :: i, identifier_count, operator_count, paren_count, unknown_count, number_count
        logical :: has_assignment, has_function_call, has_invalid_chars

        identifier_count = 0
        operator_count = 0
        paren_count = 0
        unknown_count = 0
        number_count = 0
        has_assignment = .false.
        has_function_call = .false.
        has_invalid_chars = .false.

        do i = 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_IDENTIFIER)
                identifier_count = identifier_count + 1
                ! Check for function call pattern: identifier followed by (
                if (i < size(tokens) .and. tokens(i + 1)%text == "(") then
                    has_function_call = .true.
                end if
            case (TK_NUMBER)
                number_count = number_count + 1
            case (TK_OPERATOR)
                operator_count = operator_count + 1
                ! Check for assignment operator
                if (tokens(i)%text == "=") then
                    has_assignment = .true.
                end if
                ! Check for parentheses in operator tokens
                if (tokens(i)%text == "(" .or. tokens(i)%text == ")") then
                    paren_count = paren_count + 1
                end if
            case (TK_UNKNOWN)
                unknown_count = unknown_count + 1
                ! Check for clearly invalid characters
                if (len(tokens(i)%text) > 0) then
                    if (index(tokens(i)%text, '***') > 0 .or. &
                        index(tokens(i)%text, '@') > 0 .or. &
                        index(tokens(i)%text, '#') > 0) then
                        has_invalid_chars = .true.
                    end if
                end if
            end select
        end do

        ! Reject if contains invalid characters or too many unknown tokens
        if (has_invalid_chars .or. unknown_count > 2) then
            is_expression = .false.
            ! Consider it a valid expression if:
            ! - Has identifiers and operators (mathematical expression)
            ! - Has assignment pattern (variable assignment)
            ! - Has function call pattern
            ! - Has numbers and operators (numerical expression like "2 + 3")
            ! - Has reasonable balance of tokens
        else
            is_expression = ((identifier_count > 0) .or. (number_count > 0 .and. operator_count > 0)) .and. &
                            (has_assignment .or. has_function_call .or. &
                             (operator_count > 0 .and. (identifier_count + number_count) >= operator_count))
        end if
    end function is_likely_fortran_expression

    logical function is_valid_operand_token(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid_operand_token = .false.
        select case (token%kind)
        case (TK_IDENTIFIER, TK_NUMBER, TK_STRING)
            is_valid_operand_token = .true.
        case (TK_KEYWORD)
            ! Statement keywords cannot continue an expression.  Keep logical
            ! literals as the keyword-shaped operands they are.
            if (.not. allocated(token%text)) return
            lowered = to_lower(trim(token%text))
            is_valid_operand_token = lowered == '.true.' .or. &
                                     lowered == '.false.' .or. &
                                     index(lowered, '.true._') == 1 .or. &
                                     index(lowered, '.false._') == 1
        case (TK_OPERATOR)
            if (.not. allocated(token%text)) return
            select case (trim(token%text))
            case ("(", "[", "+", "-", ".not.")
                is_valid_operand_token = .true.
            end select
        end select
    end function is_valid_operand_token
