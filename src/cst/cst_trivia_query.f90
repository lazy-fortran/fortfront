module cst_trivia_query
    use cst_arena, only: cst_arena_t
    use cst_nodes, only: cst_node_t, trivia_t, CST_COMMENT, CST_NEWLINE, &
                         CST_WHITESPACE
    use lexer_core, only: token_t, trivia_token_t, tokenize_core_with_trivia, &
                          TK_COMMENT, TK_NEWLINE, TK_WHITESPACE
    use ast_arena_modern, only: ast_arena_t
    use ast_introspection, only: get_node_source_location_from_arena
    implicit none
    private

    public :: get_cst_node_for_ast
    public :: get_leading_trivia
    public :: get_trailing_trivia
    public :: get_source_trivia_at
    public :: get_trivia_for_ast_node
    public :: get_trivia_for_ast_node_tokens

contains

    function get_cst_node_for_ast(cst_arena, ast_index) result(cst_index)
        type(cst_arena_t), intent(in) :: cst_arena
        integer, intent(in) :: ast_index
        integer :: cst_index

        integer :: i

        cst_index = 0
        if (ast_index <= 0) return
        if (.not. allocated(cst_arena%nodes)) return

        do i = 1, cst_arena%size
            if (cst_arena%nodes(i)%ast_link == ast_index) then
                cst_index = i
                return
            end if
        end do
    end function get_cst_node_for_ast

    subroutine get_leading_trivia(cst_arena, cst_index, trivia)
        type(cst_arena_t), intent(in) :: cst_arena
        integer, intent(in) :: cst_index
        type(trivia_t), allocatable, intent(out) :: trivia(:)

        type(cst_node_t) :: node

        if (cst_index < 1 .or. cst_index > cst_arena%size) then
            allocate (trivia(0))
            return
        end if

        node = cst_arena%nodes(cst_index)
        if (allocated(node%leading_trivia)) then
            trivia = node%leading_trivia
        else
            allocate (trivia(0))
        end if
    end subroutine get_leading_trivia

    subroutine get_trailing_trivia(cst_arena, cst_index, trivia)
        type(cst_arena_t), intent(in) :: cst_arena
        integer, intent(in) :: cst_index
        type(trivia_t), allocatable, intent(out) :: trivia(:)

        type(cst_node_t) :: node

        if (cst_index < 1 .or. cst_index > cst_arena%size) then
            allocate (trivia(0))
            return
        end if

        node = cst_arena%nodes(cst_index)
        if (allocated(node%trailing_trivia)) then
            trivia = node%trailing_trivia
        else
            allocate (trivia(0))
        end if
    end subroutine get_trailing_trivia

    function get_source_trivia_at(source, line, column) result(trivia)
        character(len=*), intent(in) :: source
        integer, intent(in) :: line, column
        type(trivia_t), allocatable :: trivia(:)

        character(len=:), allocatable :: normalized
        integer :: pos, start_pos, end_pos
        character :: ch

        normalized = normalize_to_lf(source)

        pos = line_column_to_pos(normalized, line, column)
        if (pos <= 0 .or. pos > len(normalized)) then
            allocate (trivia(0))
            return
        end if

        ch = normalized(pos:pos)
        if (ch == ' ' .or. ch == char(9)) then
            start_pos = pos
            end_pos = pos
            do while (end_pos <= len(normalized))
                ch = normalized(end_pos:end_pos)
                if (ch /= ' ' .and. ch /= char(9)) exit
                end_pos = end_pos + 1
            end do
            end_pos = end_pos - 1
            allocate (trivia(1))
            trivia(1) = make_trivia(CST_WHITESPACE, normalized(start_pos:end_pos), &
                                    start_pos, end_pos)
        else if (ch == '!') then
            start_pos = pos
            end_pos = pos
            do while (end_pos <= len(normalized))
                ch = normalized(end_pos:end_pos)
                if (ch == new_line('A')) exit
                end_pos = end_pos + 1
            end do
            end_pos = end_pos - 1
            if (end_pos < start_pos) then
                allocate (trivia(0))
                return
            end if
            allocate (trivia(1))
            trivia(1) = make_trivia(CST_COMMENT, normalized(start_pos:end_pos), &
                                    start_pos, end_pos)
        else if (ch == new_line('A')) then
            allocate (trivia(1))
            trivia(1) = make_trivia(CST_NEWLINE, new_line('A'), pos, pos)
        else
            allocate (trivia(0))
        end if
    end function get_source_trivia_at

    subroutine get_trivia_for_ast_node(source, ast_arena, ast_index, leading, &
                                       trailing, found)
        character(len=*), intent(in) :: source
        type(ast_arena_t), intent(in) :: ast_arena
        integer, intent(in) :: ast_index
        type(trivia_t), allocatable, intent(out) :: leading(:)
        type(trivia_t), allocatable, intent(out) :: trailing(:)
        logical, intent(out) :: found

        type(token_t), allocatable :: tokens(:)
        call tokenize_core_with_trivia(source, tokens)
        call get_trivia_for_ast_node_tokens(tokens, ast_arena, &
                                            ast_index, leading, &
                                            trailing, found)
    end subroutine get_trivia_for_ast_node

    subroutine get_trivia_for_ast_node_tokens(tokens, ast_arena, ast_index, &
                                              leading, trailing, found)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(in) :: ast_arena
        integer, intent(in) :: ast_index
        type(trivia_t), allocatable, intent(out) :: leading(:)
        type(trivia_t), allocatable, intent(out) :: trailing(:)
        logical, intent(out) :: found

        integer :: node_line, node_col
        integer :: token_index

        allocate (leading(0))
        allocate (trailing(0))
        found = .false.

        call get_node_source_location_from_arena(ast_arena, ast_index, &
                                                 node_line, &
                                                 node_col)
        if (node_line <= 0 .or. node_col <= 0) return

        token_index = find_token_at(tokens, node_line, node_col)
        if (token_index <= 0) return

        call convert_trivia_tokens(tokens(token_index)%leading_trivia, leading)
        call &
            convert_trivia_tokens(tokens(token_index)%trailing_trivia, &
                                  trailing)
        found = .true.
    end subroutine get_trivia_for_ast_node_tokens

    function find_token_at(tokens, line, column) result(token_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: line, column
        integer :: token_index

        integer :: i

        token_index = 0
        do i = 1, size(tokens)
            if (tokens(i)%kind == 0) cycle
            if (tokens(i)%kind == -1) cycle
            if (tokens(i)%kind == TK_COMMENT) cycle
            if (tokens(i)%kind == TK_NEWLINE) cycle
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            if (tokens(i)%line == line .and. tokens(i)%column == column) then
                token_index = i
                return
            end if
        end do
    end function find_token_at

    subroutine convert_trivia_tokens(tokens, trivia)
        type(trivia_token_t), allocatable, intent(in) :: tokens(:)
        type(trivia_t), allocatable, intent(out) :: trivia(:)

        integer :: i
        integer :: trivia_kind

        if (.not. allocated(tokens)) then
            allocate (trivia(0))
            return
        end if

        allocate (trivia(size(tokens)))
        do i = 1, size(tokens)
            trivia_kind = trivia_kind_from_token(tokens(i)%kind)
            trivia(i) = make_trivia(trivia_kind, tokens(i)%text, 0, 0)
        end do
    end subroutine convert_trivia_tokens

    integer function trivia_kind_from_token(token_kind)
        integer, intent(in) :: token_kind

        select case (token_kind)
        case (TK_WHITESPACE)
            trivia_kind_from_token = CST_WHITESPACE
        case (TK_COMMENT)
            trivia_kind_from_token = CST_COMMENT
        case (TK_NEWLINE)
            trivia_kind_from_token = CST_NEWLINE
        case default
            trivia_kind_from_token = 0
        end select
    end function trivia_kind_from_token

    function make_trivia(kind, text, start_pos, end_pos) result(trivia)
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos, end_pos
        type(trivia_t) :: trivia

        trivia%kind = kind
        trivia%text = text
        trivia%start_pos = start_pos
        trivia%end_pos = end_pos
    end function make_trivia

    function normalize_to_lf(source) result(normalized)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: normalized
        character(len=:), allocatable :: buffer
        integer :: src_len, i, write_pos
        character :: ch

        src_len = len(source)
        if (src_len == 0) then
            allocate (character(len=0) :: normalized)
            return
        end if

        allocate (character(len=src_len) :: buffer)
        write_pos = 0

        do i = 1, src_len
            ch = source(i:i)
            select case (ch)
            case (char(13))
                if (i < src_len) then
                    if (source(i + 1:i + 1) == char(10)) cycle
                end if
                write_pos = write_pos + 1
                buffer(write_pos:write_pos) = char(10)
            case default
                write_pos = write_pos + 1
                buffer(write_pos:write_pos) = ch
            end select
        end do

        if (write_pos <= 0) then
            allocate (character(len=0) :: normalized)
        else
            allocate (character(len=write_pos) :: normalized)
            normalized = buffer(1:write_pos)
        end if
    end function normalize_to_lf

    integer function line_column_to_pos(source, line, column)
        character(len=*), intent(in) :: source
        integer, intent(in) :: line, column

        integer :: current_line, current_col
        integer :: i
        character :: ch

        line_column_to_pos = 0
        if (line <= 0 .or. column <= 0) return

        current_line = 1
        current_col = 1

        do i = 1, len(source)
            if (current_line == line .and. current_col == column) then
                line_column_to_pos = i
                return
            end if

            ch = source(i:i)
            if (ch == new_line('A') .or. ch == char(13)) then
                current_line = current_line + 1
                current_col = 1
            else
                current_col = current_col + 1
            end if
        end do
    end function line_column_to_pos

end module cst_trivia_query
