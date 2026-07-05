module parser_common_statement_module
    ! Parse COMMON statements into structured common_block_node AST nodes.
    ! Handles blank common, named blocks, and several blocks in one statement:
    !   common /a/ x, y, /b/ z      ! two named blocks
    !   common w, v                 ! blank common
    !   common // p, /a/ q          ! explicit blank then named
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, TK_OPERATOR, TK_KEYWORD, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_data, only: declaration_node
    use ast_factory, only: push_common_block
    use parser_declaration_attributes_module, only: parse_array_dimensions
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
                call parse_member_array_spec(parser, arena, trim(token%text))
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
            if (is_block_name(token)) then
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

    ! A COMMON member may carry an array-spec, e.g. `common /b/ arr(10)`.
    ! The extent is not a separate declaration: it upgrades the companion
    ! type declaration in the same scope to a rank-N array (F2018 8.10.2.2),
    ! the way an ALLOCATABLE statement upgrades a scalar declaration.
    subroutine parse_member_array_spec(parser, arena, name)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(token_t) :: token
        integer, allocatable :: dimension_indices(:)

        call skip_inline_trivia(parser)
        if (parser%is_at_end()) return
        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. trim(token%text) == "(")) return

        token = parser%consume()
        call parse_array_dimensions(parser, arena, dimension_indices)
        if (.not. allocated(dimension_indices)) allocate (dimension_indices(0))
        if (size(dimension_indices) > 0) then
            call apply_common_array_shape(arena, name, dimension_indices)
        end if
    end subroutine parse_member_array_spec

    subroutine apply_common_array_shape(arena, name, dimension_indices)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension_indices(:)
        character(len=:), allocatable :: target, decl_name
        integer :: idx, i

        target = to_lower(adjustl(trim(name)))

        do idx = arena%size, 1, -1
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do i = 1, size(decl%var_names)
                        decl_name = to_lower(trim(decl%var_names(i)))
                        if (decl_name == target) then
                            decl%is_array = .true.
                            call set_common_dimensions(decl, dimension_indices)
                            arena%entries(idx)%node = decl
                            return
                        end if
                    end do
                end if
                if (allocated(decl%var_name)) then
                    decl_name = to_lower(trim(decl%var_name))
                    if (decl_name == target) then
                        decl%is_array = .true.
                        call set_common_dimensions(decl, dimension_indices)
                        arena%entries(idx)%node = decl
                        return
                    end if
                end if
            end select
        end do
    end subroutine apply_common_array_shape

    subroutine set_common_dimensions(decl, dimension_indices)
        type(declaration_node), intent(inout) :: decl
        integer, intent(in) :: dimension_indices(:)

        if (allocated(decl%dimension_indices)) deallocate (decl%dimension_indices)
        allocate (decl%dimension_indices(size(dimension_indices)))
        decl%dimension_indices = dimension_indices
    end subroutine set_common_dimensions

    logical function is_slash(token)
        type(token_t), intent(in) :: token
        is_slash = token%kind == TK_OPERATOR .and. &
            (trim(token%text) == "/" .or. trim(token%text) == "//")
    end function is_slash

    logical function is_block_name(token)
        type(token_t), intent(in) :: token
        is_block_name = token%kind == TK_IDENTIFIER .or. &
            token%kind == TK_KEYWORD
    end function is_block_name

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
