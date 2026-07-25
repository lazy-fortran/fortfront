module parser_type_specifications_module
    ! Type specification parsing module for implicit statements
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_NUMBER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use parser_implicit_shared_module, only: parse_none_spec_list
    use parser_implicit_letter_specs_module, only: validate_implicit_letter_specs
    implicit none
    private
    integer, allocatable, save :: implicit_additional_indices(:)

    type :: implicit_spec_info_t
        character(len=:), allocatable :: type_name
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: has_length = .false.
        integer :: length_value = 0
        character(len=64), allocatable :: letter_ranges(:)
    end type implicit_spec_info_t

    public :: parse_implicit_statement
    public :: take_implicit_additional_indices

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
                    token = parser%consume() ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        token = parser%consume() ! consume end letter
                    end if
                end if

                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume() ! consume ','
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
                    token = parser%consume() ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        letter_ranges(i) = trim(letter_ranges(i))//"-"//token%text
                        token = parser%consume() ! consume end letter
                    end if
                end if

                i = i + 1

                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume() ! consume ','
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
            token = parser%consume() ! consume 'len'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume() ! consume '='
                token = parser%peek()
            end if
        end if
        if (token%text == "*") then
            length_value = -1
            has_length = .true.
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            read (token%text, *) length_value
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
            read (token%text, *) kind_value
            has_kind = .true.
            token = parser%consume()
        end if
    end subroutine parse_kind_specification

    ! Check if parentheses contain type specification (extracted logic)
    function is_type_parameter_specification(parser, type_name) &
            result(is_type_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: type_name
        logical :: is_type_spec
        type(token_t) :: token
        integer :: saved_pos

        is_type_spec = .false.
        saved_pos = parser%current_token

        token = parser%consume() ! consume '('
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
    end function is_type_parameter_specification

    function parse_implicit_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer :: line, column
        type(implicit_spec_info_t), allocatable :: specs(:)
        integer :: spec_count

        call clear_implicit_additional_indices()

        ! Consume 'implicit' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        if (try_parse_implicit_none(parser, arena, line, column, stmt_index)) then
            call clear_implicit_additional_indices()
            return
        end if

        call validate_implicit_letter_specs(parser)

        call parse_implicit_spec_sequence(parser, specs, spec_count)
        if (spec_count <= 0) then
            stmt_index = 0
            call clear_implicit_additional_indices()
            return
        end if

        call push_implicit_specs(arena, specs, line, column, stmt_index)
    end function parse_implicit_statement

    subroutine clear_implicit_additional_indices()
        if (allocated(implicit_additional_indices)) then
            deallocate (implicit_additional_indices)
        end if
    end subroutine clear_implicit_additional_indices

    logical function try_parse_implicit_none(parser, arena, line, column, stmt_index) &
            result(parsed)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line
        integer, intent(in) :: column
        integer, intent(out) :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: none_spec

        parsed = .false.
        stmt_index = 0

        token = parser%peek()
        if (token%kind /= TK_KEYWORD .or. token%text /= "none") return

        token = parser%consume()
        none_spec = ""

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_none_spec_list(parser, none_spec)
        end if

        if (allocated(none_spec) .and. len_trim(none_spec) == 0) then
            deallocate (none_spec)
        end if

        if (allocated(none_spec)) then
            stmt_index = push_implicit_statement(arena, .true., line=line, &
                column=column, parent_index=0, &
                none_spec=none_spec)
        else
            stmt_index = push_implicit_statement(arena, .true., line=line, &
                column=column, parent_index=0)
        end if

        parsed = (stmt_index > 0)
    end function try_parse_implicit_none

    subroutine parse_implicit_spec_sequence(parser, specs, spec_count)
        type(parser_state_t), intent(inout) :: parser
        type(implicit_spec_info_t), allocatable, intent(out) :: specs(:)
        integer, intent(out) :: spec_count
        type(token_t) :: token
        type(implicit_spec_info_t) :: spec
        logical :: success

        spec_count = 0
        allocate (specs(0))

        do
            success = parse_implicit_single_spec(parser, spec)
            if (.not. success) exit

            spec_count = spec_count + 1
            call append_implicit_spec(specs, spec_count, spec)

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                cycle
            end if
            exit
        end do
    end subroutine parse_implicit_spec_sequence

    logical function parse_implicit_single_spec(parser, spec) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(implicit_spec_info_t), intent(out) :: spec
        type(token_t) :: token
        integer :: saved_pos
        integer :: range_count
        character(len=64), allocatable :: tmp_ranges(:)

        success = .false.

        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return

        spec%type_name = token%text
        token = parser%consume()

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            saved_pos = parser%current_token
            if (is_type_parameter_specification(parser, spec%type_name)) then
                token = parser%consume()
                if (spec%type_name == "character") then
                    call parse_character_type_spec(parser, spec%length_value, &
                        spec%has_length)
                else
                    call parse_kind_specification(parser, spec%kind_value, &
                        spec%has_kind)
                end if

                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            else
                parser%current_token = saved_pos
            end if
        end if

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call count_letter_ranges(parser, range_count)

            if (range_count > 0) then
                allocate (tmp_ranges(range_count))
                call parse_letter_ranges(parser, tmp_ranges)
                allocate (spec%letter_ranges(range_count))
                spec%letter_ranges = tmp_ranges
                deallocate (tmp_ranges)
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        end if

        success = .true.
    end function parse_implicit_single_spec

    subroutine append_implicit_spec(specs, count, spec)
        type(implicit_spec_info_t), allocatable, intent(inout) :: specs(:)
        integer, intent(in) :: count
        type(implicit_spec_info_t), intent(in) :: spec
        type(implicit_spec_info_t), allocatable :: temp(:)
        integer :: old_size

        if (.not. allocated(specs)) then
            allocate (specs(count))
            specs(count) = spec
            return
        end if

        old_size = size(specs)
        if (old_size < count) then
            allocate (temp(count))
            if (old_size > 0) temp(1:old_size) = specs
            temp(count) = spec
            call move_alloc(temp, specs)
        else
            specs(count) = spec
        end if
    end subroutine append_implicit_spec

    subroutine push_implicit_specs(arena, specs, line, column, stmt_index)
        type(ast_arena_t), intent(inout) :: arena
        type(implicit_spec_info_t), intent(in) :: specs(:)
        integer, intent(in) :: line
        integer, intent(in) :: column
        integer, intent(out) :: stmt_index
        integer :: spec_count, i
        integer, allocatable :: created_indices(:)

        spec_count = size(specs)
        allocate (created_indices(spec_count))

        do i = 1, spec_count
            associate (spec => specs(i))
                if (allocated(spec%letter_ranges)) then
                    created_indices(i) = push_implicit_statement(arena, .false., &
                        type_name=spec%type_name, &
                        kind_value=spec%kind_value, &
                        has_kind=spec%has_kind, &
                        length_value=spec%length_value, &
                        has_length=spec%has_length, &
                        letter_ranges=spec%letter_ranges, &
                        line=line, &
                        column=column, &
                        parent_index=0)
                else
                    created_indices(i) = push_implicit_statement(arena, .false., &
                        type_name=spec%type_name, &
                        kind_value=spec%kind_value, &
                        has_kind=spec%has_kind, &
                        length_value=spec%length_value, &
                        has_length=spec%has_length, &
                        line=line, &
                        column=column, &
                        parent_index=0)
                end if
            end associate
        end do

        stmt_index = created_indices(1)
        if (spec_count > 1) then
            call clear_implicit_additional_indices()
            allocate (implicit_additional_indices(spec_count - 1))
            implicit_additional_indices = created_indices(2:)
        else
            call clear_implicit_additional_indices()
        end if
    end subroutine push_implicit_specs

    function take_implicit_additional_indices() result(indices)
        integer, allocatable :: indices(:)

        if (allocated(implicit_additional_indices)) then
            call move_alloc(implicit_additional_indices, indices)
        else
            allocate (indices(0))
        end if
    end function take_implicit_additional_indices

end module parser_type_specifications_module
