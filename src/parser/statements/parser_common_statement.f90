module parser_common_statement_module
    ! Parse COMMON statements into structured common_block_node AST nodes.
    ! Handles blank common, named blocks, and several blocks in one statement:
    !   common /a/ x, y, /b/ z      ! two named blocks
    !   common w, v                 ! blank common
    !   common // p, /a/ q          ! explicit blank then named
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, TK_OPERATOR, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_factory, only: push_common_block
    implicit none
    private

    public :: parse_common_statement

contains

    integer function parse_common_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        type(string_t), allocatable :: block_names(:)
        type(string_t), allocatable :: member_names(:)
        integer, allocatable :: member_block(:)
        integer :: line, column, current_block

        stmt_index = 0
        allocate (block_names(0))
        allocate (member_names(0))
        allocate (member_block(0))

        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume() ! consume "common"

        ! Implicit blank common (block 1) used until a /name/ appears
        current_block = 0

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            if (parser%is_at_end()) exit
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit

            if (is_slash(token)) then
                call parse_block_header(parser, block_names, current_block)
            else if (token%kind == TK_OPERATOR .and. trim(token%text) == ",") then
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER) then
                if (current_block == 0) then
                    block_names = [block_names, string_t("")]
                    current_block = size(block_names)
                end if
                member_names = [member_names, string_t(trim(token%text))]
                member_block = [member_block, current_block]
                token = parser%consume()
            else
                token = parser%consume()
            end if
        end do

        stmt_index = push_common_block(arena, block_names, member_names, &
                                       member_block, line=line, column=column)
    end function parse_common_statement

    subroutine parse_block_header(parser, block_names, current_block)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: block_names(:)
        integer, intent(inout) :: current_block
        type(token_t) :: token
        character(len=:), allocatable :: name

        token = parser%consume() ! consume "/" or "//"
        name = ""
        if (trim(token%text) /= "//") then
            ! "/name/" form: read name then closing slash
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                name = trim(token%text)
                token = parser%consume()
            end if
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (is_slash(token)) token = parser%consume()
        end if
        block_names = [block_names, string_t(name)]
        current_block = size(block_names)
    end subroutine parse_block_header

    logical function is_slash(token)
        type(token_t), intent(in) :: token
        is_slash = token%kind == TK_OPERATOR .and. &
                   (trim(token%text) == "/" .or. trim(token%text) == "//")
    end function is_slash

    subroutine skip_inline_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_inline_trivia

end module parser_common_statement_module
