
    subroutine initialize_if_hooks()
        if (.not. if_hooks_initialized) then
            call register_parse_do_loop(parse_do_loop)
            ! Also as the fallback for a caller that populated no do
            ! callback, which is how a loop inside a case arm is reached.
            call register_fallback_do_parser(parse_any_do)
            ! parse_if is in scope here through parser_if_constructs, which
            ! this module already uses, so registering it costs no new
            ! dependency and closes no cycle.
            call register_fallback_if_parser(parse_if)
            if_hooks_initialized = .true.
        end if
    end subroutine initialize_if_hooks

    function parse_any_do(parser, arena) result(loop_index)
        !! Either shape of DO, chosen by what follows the keyword.
        !!
        !! The fallback has to cover `do while` as well as a counted loop:
        !! `select case` populates no do callback at all, so both forms reach
        !! this and registering only the counted parser left the while form
        !! reported as an unrecognised statement.
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index
        type(token_t) :: next_token

        ! peek() is the current token, so look one past it directly.
        next_token%kind = 0
        if (parser%current_token + 1 <= size(parser%tokens)) &
            next_token = parser%tokens(parser%current_token + 1)
        if (next_token%kind == TK_KEYWORD) then
            if (to_lower(trim(next_token%text)) == "while") then
                loop_index = parse_do_while(parser, arena)
                return
            end if
        end if
        loop_index = parse_do_loop(parser, arena)
    end function parse_any_do

    subroutine ensure_if_do_registration()
        call initialize_if_hooks()
    end subroutine ensure_if_do_registration

    subroutine append_control(control_list, control)
        type(do_loop_control_t), allocatable, intent(inout) :: control_list(:)
        type(do_loop_control_t), intent(in) :: control

        if (.not. allocated(control_list)) then
            allocate (control_list(1))
            control_list(1) = control
        else
            control_list = [control_list, control]
        end if
    end subroutine append_control

    logical function do_has_no_control(parser) result(bare)
        !! Whether the DO keyword is followed by nothing on its line.
        type(parser_state_t), intent(in) :: parser
        integer :: idx

        bare = .true.
        idx = parser%current_token
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case (TK_NEWLINE, TK_EOF)
                return
            case default
                bare = .false.
                return
            end select
        end do
    end function do_has_no_control

    logical function next_tokens_form_control(parser, start_pos) result(is_control)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: start_pos
        integer :: idx, paren_depth

        idx = start_pos
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) then
            is_control = .false.
            return
        end if

        ! Skip optional type-spec prefix: type-name [ (params) ] ::
        if (parser%tokens(idx)%kind == TK_KEYWORD) then
            select case (trim(to_lower(parser%tokens(idx)%text)))
            case ("integer", "real", "logical", "character", "complex", "double")
                idx = idx + 1
                ! Skip "precision" after "double"
                if (idx <= size(parser%tokens)) then
                    if (parser%tokens(idx)%kind == TK_KEYWORD .and. &
                        trim(to_lower(parser%tokens(idx)%text)) == "precision") then
                        idx = idx + 1
                    end if
                end if
                ! Skip parenthesized kind spec
                if (idx <= size(parser%tokens)) then
                    if (parser%tokens(idx)%kind == TK_OPERATOR .and. &
                        parser%tokens(idx)%text == "(") then
                        paren_depth = 1
                        idx = idx + 1
                        do while (idx <= size(parser%tokens) .and. paren_depth > 0)
                            if (parser%tokens(idx)%kind == TK_OPERATOR) then
                                if (parser%tokens(idx)%text == "(") then
                                    paren_depth = paren_depth + 1
                                else if (parser%tokens(idx)%text == ")") then
                                    paren_depth = paren_depth - 1
                                end if
                            end if
                            idx = idx + 1
                        end do
                    end if
                end if
                ! Skip ::
                if (idx <= size(parser%tokens)) then
                    if (parser%tokens(idx)%kind == TK_OPERATOR .and. &
                        parser%tokens(idx)%text == "::") then
                        idx = idx + 1
                    end if
                end if
            end select
        end if

        ! Skip whitespace after type-spec
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) then
            is_control = .false.
            return
        end if

        if (parser%tokens(idx)%kind /= TK_IDENTIFIER) then
            is_control = .false.
            return
        end if

        idx = idx + 1
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) then
            is_control = .false.
            return
        end if

        if (parser%tokens(idx)%kind == TK_OPERATOR .and. &
            parser%tokens(idx)%text == "=") then
            is_control = .true.
        else
            is_control = .false.
        end if
    end function next_tokens_form_control

    ! A DO iterator separates its bounds with "," (F2023 R1125) or, for the
    ! range form fortfront also accepts, with ":".
    logical function is_iterator_separator(tok) result(is_separator)
        type(token_t), intent(in) :: tok

        is_separator = .false.
        if (tok%kind /= TK_OPERATOR) return
        is_separator = tok%text == "," .or. tok%text == ":"
    end function is_iterator_separator

    logical function parse_standard_control(parser, arena, control) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), intent(out) :: control
        type(token_t) :: tok
        logical :: allow_colon_step

        success = .false.
        control%step_index = 0

        ! Fortran reserves no words, so the iterator may be spelled with one:
        ! `do parameter = 1, n` is legal. What follows settles it - only an
        ! iterator has `=` here, where `do while (` and `do concurrent (` have
        ! an opening parenthesis and fall through to their own parsers.
        tok = parser%consume()
        if (tok%kind /= TK_IDENTIFIER .and. tok%kind /= TK_KEYWORD) return
        control%var_name = tok%text

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= "=") return

        ! Past the "name =" prefix this statement can only be a DO iterator
        ! (F2023 R1125 loop-control), so a malformed remainder is a hard
        ! syntax error rather than a silent parse failure.
        control%start_index = parse_logical_or(parser, arena)
        if (control%start_index <= 0) then
            call parser%error("Syntax error in DO iterator: expected an "// &
                "initial loop bound expression")
            return
        end if

        tok = parser%peek()
        if (.not. is_iterator_separator(tok)) then
            call parser%error_at_token("Syntax error in DO iterator: expected "// &
                "',' after the initial loop bound", tok, &
                suggestion="write the iterator as do var = start, end [, step]")
            return
        end if
        tok = parser%consume()
        allow_colon_step = tok%text == ":"

        control%end_index = parse_logical_or(parser, arena)
        if (control%end_index <= 0) then
            call parser%error("Syntax error in DO iterator: expected a "// &
                "terminal loop bound expression")
            return
        end if

        if (.not. parser%is_at_end()) then
            tok = parser%peek()
            if (tok%kind == TK_OPERATOR) then
                if (tok%text == ",") then
                    tok = parser%consume()
                    control%step_index = parse_logical_or(parser, arena)
                    if (control%step_index <= 0) return
                else if (allow_colon_step .and. tok%text == ":") then
                    tok = parser%consume()
                    control%step_index = parse_logical_or(parser, arena)
                    if (control%step_index <= 0) return
                end if
            end if
        end if

        if (control%step_index < 0) control%step_index = 0
        success = control%start_index > 0 .and. control%end_index > 0
    end function parse_standard_control

    logical function parse_concurrent_control(parser, arena, control) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), intent(out) :: control
        type(token_t) :: tok
        character(len=:), allocatable :: type_spec_str
        integer :: saved_pos

        success = .false.
        control%step_index = 0
        type_spec_str = ""

        saved_pos = parser%current_token

        ! Check for optional type-spec: integer, real, logical, character, complex, double
        tok = parser%peek()
        if (tok%kind == TK_KEYWORD) then
            select case (trim(to_lower(tok%text)))
            case ("integer", "real", "logical", "character", "complex", "double")
                tok = parser%consume()
                type_spec_str = trim(tok%text)

                ! Handle "double precision"
                if (trim(to_lower(tok%text)) == "double") then
                    tok = parser%peek()
                    if (tok%kind == TK_KEYWORD .and. &
                        trim(to_lower(tok%text)) == "precision") then
                        tok = parser%consume()
                        type_spec_str = "double precision"
                    end if
                end if

                ! Handle kind parameters: type(kind) or type(param)
                tok = parser%peek()
                if (tok%kind == TK_OPERATOR .and. tok%text == "(") then
                    type_spec_str = trim(type_spec_str) // parse_type_spec_parens(parser)
                end if

                ! Expect ::
                tok = parser%peek()
                if (tok%kind == TK_OPERATOR .and. tok%text == "::") then
                    tok = parser%consume()
                else
                    ! Not a type-spec form, restore position
                    parser%current_token = saved_pos
                    type_spec_str = ""
                end if
            end select
        end if

        ! Parse variable name
        ! A keyword is a legal iterator name; see the note above.
        tok = parser%consume()
        if (tok%kind /= TK_IDENTIFIER .and. tok%kind /= TK_KEYWORD) then
            if (len_trim(type_spec_str) > 0) then
                parser%current_token = saved_pos
            end if
            return
        end if
        control%var_name = tok%text
        if (len_trim(type_spec_str) > 0) control%type_spec = type_spec_str

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= "=") return

        control%start_index = parse_logical_or(parser, arena)
        if (control%start_index <= 0) return

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= ":") return

        control%end_index = parse_logical_or(parser, arena)
        if (control%end_index <= 0) return

        if (.not. parser%is_at_end()) then
            tok = parser%peek()
            if (tok%kind == TK_OPERATOR .and. tok%text == ":") then
                tok = parser%consume()
                control%step_index = parse_logical_or(parser, arena)
                if (control%step_index <= 0) return
            end if
        end if

        if (control%step_index < 0) control%step_index = 0
        success = control%start_index > 0 .and. control%end_index > 0
    end function parse_concurrent_control

    ! Parse parenthesized type parameters: (kind=8) or (8) etc.
    ! Returns the full parenthesized string including parentheses
    function parse_type_spec_parens(parser) result(parens_str)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable :: parens_str
        type(token_t) :: tok
        integer :: depth
        character(len=256) :: buf

        buf = ""
        depth = 0

        do while (.not. parser%is_at_end())
            tok = parser%consume()
            if (tok%kind == TK_OPERATOR) then
                if (tok%text == "(") then
                    depth = depth + 1
                else if (tok%text == ")") then
                    depth = depth - 1
                    if (depth == 0) then
                        buf = trim(buf) // tok%text
                        exit
                    end if
                end if
            end if
            if (len_trim(buf) > 0 .and. tok%kind /= TK_OPERATOR) buf = trim(buf) // " "
            buf = trim(buf) // tok%text
        end do

        parens_str = trim(buf)
    end function parse_type_spec_parens

    logical function parse_concurrent_control_list(parser, arena, controls) &
        result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), allocatable, intent(out) :: controls(:)
        type(do_loop_control_t) :: control
        type(token_t) :: tok
        integer :: lookahead

        success = .false.
        if (allocated(controls)) then
            deallocate (controls)
        end if
        allocate (controls(0))

        if (.not. parse_concurrent_control(parser, arena, control)) return
        call append_control(controls, control)

        do
            if (parser%is_at_end()) exit
            tok = parser%peek()
            if (tok%kind /= TK_OPERATOR .or. tok%text /= ",") exit

            lookahead = parser%current_token + 1
            if (.not. next_tokens_form_control(parser, lookahead)) return

            tok = parser%consume()
            if (.not. parse_concurrent_control(parser, arena, control)) return
            call append_control(controls, control)
        end do

        success = size(controls) > 0
    end function parse_concurrent_control_list

    logical function has_then_before_newline(tokens, start_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: idx

        has_then_before_newline = .false.
        do idx = start_pos + 1, size(tokens)
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                if (to_lower(tokens(idx)%text) == "then") then
                    has_then_before_newline = .true.
                    return
                else if (to_lower(tokens(idx)%text) == "endif" .or. &
                         to_lower(tokens(idx)%text) == "end") then
                    return
                end if
            case (TK_NEWLINE, TK_EOF)
                return
            end select
        end do
    end function has_then_before_newline

    logical function is_else_if_context(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: idx

        ! `else if` is one statement on one line. An `else` on the line
        ! before an `if` opens a nested construct instead, and counting that
        ! as an else-if left its `end if` unmatched - which is why a do body
        ! containing this shape failed while the same shape inside an `if`
        ! parsed. So a newline ends the search rather than being skipped.
        is_else_if_context = .false.
        idx = pos - 1
        do while (idx >= 1)
            select case (tokens(idx)%kind)
            case (TK_NEWLINE)
                return
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx - 1
                cycle
            case (TK_KEYWORD)
                if (tokens(idx)%text == "else") then
                    is_else_if_context = .true.
                end if
                return
            case default
                return
            end select
        end do
    end function is_else_if_context

    integer function find_matching_end_if(tokens, start_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: depth, idx

        find_matching_end_if = -1
        if (.not. has_then_before_newline(tokens, start_pos)) return

        depth = 1
        idx = start_pos + 1
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                select case (tokens(idx)%text)
                case ("if")
                    if (.not. is_else_if_context(tokens, idx)) then
                        if (has_then_before_newline(tokens, idx)) depth = depth + 1
                    end if
                case ("endif")
                    depth = depth - 1
                    if (depth == 0) then
                        find_matching_end_if = idx
                        return
                    end if
                case ("end")
                    if (idx + 1 <= size(tokens)) then
                        if (tokens(idx + 1)%kind == TK_KEYWORD) then
                            if (tokens(idx + 1)%text == "if") then
                                depth = depth - 1
                                if (depth == 0) then
                                    find_matching_end_if = idx + 1
                                    return
                                end if
                            end if
                        end if
                    end if
                case ("elseif", "else")
                    ! Advance but do not change depth; skip remaining checks
                    idx = idx + 1
                    cycle
                end select
            case (TK_EOF)
                exit
            end select
            idx = idx + 1
        end do
    end function find_matching_end_if

    integer function skip_trivia_tokens(tokens, start_idx) result(next_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx
        integer :: idx

        idx = start_idx
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do
        next_idx = idx
    end function skip_trivia_tokens

    integer function find_matching_end_do(tokens, start_pos) result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos

        integer :: idx, next_idx
        integer :: depth
        type(token_t) :: token

        end_index = -1
        if (start_pos < 1 .or. start_pos > size(tokens)) return

        depth = 1
        idx = start_pos + 1
        do while (idx <= size(tokens))
            token = tokens(idx)

            ! Check for labeled DO: identifier : do
            if (token%kind == TK_IDENTIFIER .and. idx + 2 <= size(tokens)) then
                if (tokens(idx + 1)%kind == TK_OPERATOR .and. &
                    tokens(idx + 1)%text == ":" .and. &
                    tokens(idx + 2)%kind == TK_KEYWORD .and. &
                    tokens(idx + 2)%text == "do") then
                    depth = depth + 1
                    idx = idx + 2
                    cycle
                end if
            end if

            if (token%kind == TK_KEYWORD) then
                select case (token%text)
                case ("do")
                    depth = depth + 1
                case ("enddo", "end do")
                    depth = depth - 1
                    if (depth == 0) then
                        end_index = idx
                        return
                    end if
                case ("end")
                    ! Skip whitespace/newlines/comments after "end"
                    next_idx = skip_trivia_tokens(tokens, idx + 1)
                    if (next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_KEYWORD .and. &
                            tokens(next_idx)%text == "do") then
                            depth = depth - 1
                            if (depth == 0) then
                                ! Check if there's a label after "end do"
                                if (next_idx + 1 <= size(tokens) .and. &
                                    tokens(next_idx + 1)%kind == TK_IDENTIFIER) then
                                    end_index = next_idx + 1
                                else
                                    end_index = next_idx
                                end if
                                return
                            end if
                            idx = next_idx
                        end if
                    end if
                end select
            end if
            idx = idx + 1
        end do
    end function find_matching_end_do

    function build_do_body_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_do_loop => parse_do_loop
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_where => parse_where_construct
        callbacks%parse_forall => parse_forall
        callbacks%parse_associate => parse_associate
    end function build_do_body_callbacks

    recursive function parse_do_loop(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token, var_token, comma_token, label_token
        character(len=:), allocatable :: var_name, loop_label, type_spec
        logical :: is_bare_do
        type(do_loop_control_t), allocatable :: controls(:)
        type(do_loop_control_t) :: control
        integer :: start_index, end_index, step_index
        integer :: line, column
        integer :: control_count
        logical :: has_open_paren
        logical :: is_concurrent

        call initialize_if_hooks()

        step_index = 0  ! Initialize to 0 (no step)
        loop_index = 0  ! Initialize to 0 (failure) in case of early return
        is_bare_do = .false.
        is_concurrent = .false.
        type_spec = ""

        ! Starting to parse do loop

        ! Check for optional loop label at start: label: do ...
        if (parser%current_token >= 1 .and. parser%current_token + 2 <= &
            size(parser%tokens)) then
            if (parser%tokens(parser%current_token)%kind == TK_IDENTIFIER .and. &
                parser%tokens(parser%current_token + 1)%kind == TK_OPERATOR .and. &
                parser%tokens(parser%current_token + 1)%text == ":" .and. &
                parser%tokens(parser%current_token + 2)%kind == TK_KEYWORD .and. &
                parser%tokens(parser%current_token + 2)%text == "do") then
                label_token = parser%tokens(parser%current_token)
                loop_label = label_token%text
                parser%current_token = parser%current_token + 2
            end if
        end if

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        ! Check if it's a do while loop
        var_token = parser%peek()
        if (var_token%kind == TK_KEYWORD .and. var_token%text == "while") then
            ! Parse as do while loop
            if (allocated(loop_label)) then
                loop_index = parse_do_while_from_do(parser, arena, line, column, &
                                                    loop_label)
            else
                loop_index = parse_do_while_from_do(parser, arena, line, column)
            end if
            return
        end if

        allocate (controls(0))
        has_open_paren = .false.

        if (var_token%kind == TK_KEYWORD .and. var_token%text == "concurrent") then
            is_concurrent = .true.
            var_token = parser%consume()

            if (.not. parser%is_at_end()) then
                comma_token = parser%peek()
                if (comma_token%kind == TK_OPERATOR .and. comma_token%text == "(") then
                    has_open_paren = .true.
                    comma_token = parser%consume()
                end if
            end if

            if (.not. parse_concurrent_control_list(parser, arena, controls)) then
                call parser%error("Invalid DO CONCURRENT control list")
                loop_index = 0
                return
            end if

            if (has_open_paren) then
                if (parser%is_at_end()) then
                    loop_index = 0
                    return
                end if
                comma_token = parser%consume()
                if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ")") then
                    loop_index = 0
                    return
                end if
            end if

            ! Everything left on the DO CONCURRENT statement is a locality-spec.
            if (.not. parse_do_concurrent_locality_specs(parser)) then
                loop_index = 0
                return
            end if
        else if (do_has_no_control(parser)) then
            is_bare_do = .true.
            ! `do` with nothing after it is an infinite loop, ended by `exit`.
            ! A bare do at statement level is routed elsewhere, so this only
            ! matters where parse_do_loop is reached directly - a bare do
            ! inside a do while body, which fortfem has.
            control%var_name = ""
            control%start_index = 0
            control%end_index = 0
            control%step_index = 0
            call append_control(controls, control)
        else
            if (.not. parse_standard_control(parser, arena, control)) then
                loop_index = 0
                return
            end if
            call append_control(controls, control)
        end if

        control_count = size(controls)
        if (control_count <= 0) then
            loop_index = 0
            return
        end if

        control = controls(control_count)
        var_name = control%var_name
        start_index = control%start_index
        end_index = control%end_index
        step_index = control%step_index
        if (allocated(control%type_spec)) type_spec = control%type_spec

        ! A bare do has no bounds by definition, so the guard against a
        ! malformed control must not reject it.
        if (.not. is_bare_do .and. (start_index <= 0 .or. end_index <= 0)) then
            loop_index = 0
            return
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_start, stmt_end, j, token_count, next_index
            integer :: nested_index
            type(token_t), allocatable, target :: stmt_tokens(:)
            type(token_t) :: next_token
            type(statement_callbacks_t) :: callbacks

            allocate (body_indices(0))
            callbacks = build_do_body_callbacks()

            if (step_index > 0) then
                if (control_count == 1 .and. allocated(loop_label)) then
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              step_index=step_index, &
                                              body_indices=[integer ::], &
                                              loop_label=loop_label, &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column, &
                                              type_spec=type_spec)
                else
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              step_index=step_index, &
                                              body_indices=[integer ::], &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column, &
                                              type_spec=type_spec)
                end if
            else
                if (control_count == 1 .and. allocated(loop_label)) then
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              body_indices=[integer ::], &
                                              loop_label=loop_label, &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column, &
                                              type_spec=type_spec)
                else
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              body_indices=[integer ::], &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column, &
                                              type_spec=type_spec)
                end if
            end if

            if (loop_index > 0) then
                call capture_trailing_comment(parser, arena, loop_index)
            end if

            do while (parser%current_token <= size(parser%tokens))
                block
                    type(token_t) :: current_token
                    integer :: check_idx

                    current_token = parser%peek()
                    if (current_token%kind == TK_KEYWORD .and. &
                        current_token%text == "end") then
                        ! Skip whitespace/newlines/comments after "end"
                        check_idx = skip_trivia_tokens(parser%tokens, &
                                                       parser%current_token + 1)
                        if (check_idx <= size(parser%tokens)) then
                            if (parser%tokens(check_idx)%kind == TK_KEYWORD .and. &
                                parser%tokens(check_idx)%text == "do") then
                                ! Consume all tokens from "end" to "do"
                                current_token = parser%consume()  ! consume "end"
                                do while (parser%current_token < check_idx)
                                    current_token = parser%consume()  ! skip trivia
                                end do
                                current_token = parser%consume()  ! consume "do"
                                exit
                            end if
                        end if
                    end if
                end block

                stmt_start = parser%current_token

                do while (stmt_start <= size(parser%tokens) .and. &
                          parser%tokens(stmt_start)%kind == TK_OPERATOR .and. &
                          parser%tokens(stmt_start)%text == ";")
                    stmt_start = stmt_start + 1
                end do

                if (stmt_start > size(parser%tokens)) then
                    parser%current_token = stmt_start
                    exit
                end if

                ! ASSOCIATE is a full construct, not a one-line statement.
                ! Parse it against the parent stream so its body remains nested
                ! in this DO node instead of being emitted as later siblings.
                nested_index = 0
                if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                    if (to_lower(parser%tokens(stmt_start)%text) == 'associate') then
                        if (associated(callbacks%parse_associate)) then
                            nested_index = callbacks%parse_associate(parser, arena)
                        end if
                    end if
                end if
                if (nested_index > 0) then
                    body_indices = [body_indices, nested_index]
                    cycle
                end if

                if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                    if (parser%tokens(stmt_start)%text == "enddo" .or. &
                        parser%tokens(stmt_start)%text == "end do") then
                        parser%current_token = stmt_start
                        exit
                    else if (parser%tokens(stmt_start)%text == "end") then
                        ! Skip whitespace/newlines/comments after "end"
                        next_index = skip_trivia_tokens(parser%tokens, stmt_start + 1)
                        if (next_index <= size(parser%tokens)) then
                            next_token = parser%tokens(next_index)
                            if (next_token%kind == TK_KEYWORD .and. &
                                next_token%text == "do") then
                                parser%current_token = stmt_start
                                exit
                            end if
                        else
                            parser%current_token = stmt_start
                            exit
                        end if
                    end if
                end if

                stmt_end = stmt_start
                block
                    integer :: block_end, lookahead, do_pos
                    logical :: handled_block

                    handled_block = .false.

                    ! Check for label: do pattern
                    do_pos = stmt_start
                    if (parser%tokens(stmt_start)%kind == TK_IDENTIFIER .and. &
                        stmt_start + 2 <= size(parser%tokens)) then
                        if (parser%tokens(stmt_start + 1)%kind == TK_OPERATOR .and. &
                            parser%tokens(stmt_start + 1)%text == ":" .and. &
                            parser%tokens(stmt_start + 2)%kind == TK_KEYWORD .and. &
                            parser%tokens(stmt_start + 2)%text == "do") then
                            do_pos = stmt_start + 2
                        end if
                    end if

                    if (parser%tokens(do_pos)%kind == TK_KEYWORD) then
                        select case (parser%tokens(do_pos)%text)
                        case ("if")
                            block_end = find_matching_end_if(parser%tokens, do_pos)
                            if (block_end > do_pos) then
                                stmt_end = block_end
                                handled_block = .true.
                            end if
                        case ("do")
                            block_end = find_matching_end_do(parser%tokens, do_pos)
                            if (block_end > do_pos) then
                                stmt_end = block_end
                                handled_block = .true.
                            end if
                        case ("select")
                            lookahead = stmt_start + 1
                            do while (lookahead <= size(parser%tokens))
                                select case (parser%tokens(lookahead)%kind)
                                case (TK_WHITESPACE, TK_COMMENT)
                                    lookahead = lookahead + 1
                                    cycle
                                end select
                                exit
                            end do
                            if (lookahead <= size(parser%tokens)) then
                                if (parser%tokens(lookahead)%kind == TK_KEYWORD) then
                                    if (parser%tokens(lookahead)%text == "case") then
                                        block_end = find_statement_end( &
                                                    parser%tokens, stmt_start)
                                        if (block_end > stmt_start) then
                                            stmt_end = block_end
                                            handled_block = .true.
                                        end if
                                    end if
                                end if
                            end if
                        end select
                    end if

                    if (.not. handled_block) then
                        do j = stmt_start, size(parser%tokens)
                            select case (parser%tokens(j)%kind)
                            case (TK_EOF)
                                stmt_end = j
                                exit
                            case (TK_NEWLINE)
                                if (j == stmt_start) then
                                    stmt_end = j
                                else
                                    stmt_end = j - 1
                                end if
                                exit
                            case (TK_OPERATOR)
                                if (j > stmt_start) then
                                    if (parser%tokens(j)%text == ";") then
                                        stmt_end = j - 1
                                        exit
                                    end if
                                end if
                            end select
                            stmt_end = j
                        end do
                    end if
                end block

                if (stmt_end >= stmt_start) then
                    token_count = stmt_end - stmt_start + 1
                    allocate (stmt_tokens(token_count + 1))
                    stmt_tokens(1:token_count) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(token_count + 1)%kind = TK_EOF
                    stmt_tokens(token_count + 1)%text = ""
                    stmt_tokens(token_count + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(token_count + 1)%column = &
                        parser%tokens(stmt_end)%column + 1

                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: k
                        logical :: has_meaningful

                        has_meaningful = .false.
                        do k = 1, size(stmt_tokens)
                            select case (stmt_tokens(k)%kind)
                            case (TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                                cycle
                            case default
                                if (len_trim(stmt_tokens(k)%text) > 0) then
                                    has_meaningful = .true.
                                    exit
                                end if
                            end select
                        end do

                        if (.not. has_meaningful) then
                            if (stmt_end + 1 <= size(parser%tokens)) then
                                next_index = stmt_end + 1
                                next_token = parser%tokens(next_index)
                                if (next_token%kind == TK_OPERATOR .and. &
                                    next_token%text == ";") then
                                    parser%current_token = stmt_end + 2
                                else
                                    parser%current_token = stmt_end + 1
                                end if
                            else
                                parser%current_token = stmt_end + 1
                            end if
                            block
                                type(token_t), allocatable, target :: temp(:)
                                call move_alloc(stmt_tokens, temp)
                            end block
                            cycle
                        end if

                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                  loop_index, &
                                                                  callbacks, &
                                                                  parent_parser=parser)

                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                            end if
                        end do
                    end block

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end if

                if (stmt_end + 1 <= size(parser%tokens)) then
                    if (parser%tokens(stmt_end + 1)%kind == TK_OPERATOR .and. &
                        parser%tokens(stmt_end + 1)%text == ";") then
                        parser%current_token = stmt_end + 2
                    else
                        parser%current_token = stmt_end + 1
                    end if
                else
                    parser%current_token = stmt_end + 1
                end if
            end do

            if (allocated(arena%entries(loop_index)%node)) then
                select type (node => arena%entries(loop_index)%node)
                type is (do_loop_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if

            ! Consume the "end do" tokens after loop body is parsed
            block
                type(token_t) :: end_token
                integer :: skip_idx

                if (parser%current_token <= size(parser%tokens)) then
                    end_token = parser%peek()
                    if (end_token%kind == TK_KEYWORD) then
                        if (end_token%text == "enddo" .or. end_token%text == "end do") then
                            end_token = parser%consume()
                        else if (end_token%text == "end") then
                            end_token = parser%consume()  ! consume "end"
                            ! Skip whitespace/newlines/comments to find "do"
                            do while (parser%current_token <= size(parser%tokens))
                                end_token = parser%peek()
                                if (end_token%kind == TK_WHITESPACE .or. &
                                    end_token%kind == TK_NEWLINE .or. &
                                    end_token%kind == TK_COMMENT) then
                                    end_token = parser%consume()
                                else
                                    exit
                                end if
                            end do
                            ! Consume "do"
                            if (parser%current_token <= size(parser%tokens)) then
                                end_token = parser%peek()
                                if (end_token%kind == TK_KEYWORD .and. &
                                    end_token%text == "do") then
                                    end_token = parser%consume()
                                end if
                            end if
                        end if
                    end if
                end if
            end block
        end block

        if (control_count > 1) then
            block
                integer :: idx, outer_index, outer_step
                integer :: outer_start, outer_end
                character(len=:), allocatable :: outer_name, outer_type_spec

                do idx = control_count - 1, 1, -1
                    outer_name = controls(idx)%var_name
                    outer_start = controls(idx)%start_index
                    outer_end = controls(idx)%end_index
                    outer_step = controls(idx)%step_index
                    outer_type_spec = ""
                    if (allocated(controls(idx)%type_spec)) &
                        outer_type_spec = controls(idx)%type_spec

                    if (outer_start <= 0 .or. outer_end <= 0) then
                        loop_index = 0
                        return
                    end if

                    if (outer_step > 0) then
                        if (idx == 1 .and. allocated(loop_label)) then
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       step_index=outer_step, &
                                                       body_indices=[loop_index], &
                                                       loop_label=loop_label, &
                                                       line=line, column=column, &
                                                       type_spec=outer_type_spec)
                        else
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       step_index=outer_step, &
                                                       body_indices=[loop_index], &
                                                       line=line, column=column, &
                                                       type_spec=outer_type_spec)
                        end if
                    else
                        if (idx == 1 .and. allocated(loop_label)) then
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       body_indices=[loop_index], &
                                                       loop_label=loop_label, &
                                                       line=line, column=column, &
                                                       type_spec=outer_type_spec)
                        else
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       body_indices=[loop_index], &
                                                       line=line, column=column, &
                                                       type_spec=outer_type_spec)
                        end if
                    end if

                    if (outer_index <= 0) then
                        loop_index = 0
                        return
                    end if

                    loop_index = outer_index
                end do
            end block
        end if

    end function parse_do_loop

    function parse_do_while(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token
        integer :: line, column

        call initialize_if_hooks()

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        loop_index = parse_do_while_from_do(parser, arena, line, column)
    end function parse_do_while
