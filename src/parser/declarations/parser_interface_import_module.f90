module parser_interface_import_module
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_import_statement
    use parser_state_module, only: parser_state_t
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_WHITESPACE
    implicit none
    private
    public :: parse_import_statement
contains

    function parse_import_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        character(len=:), allocatable :: import_names(:)
        logical :: has_double_colon, is_all, is_none
        integer :: line, column
        type(token_t) :: token
        character(len=:), allocatable :: lowered_text
        integer :: name_count

        stmt_index = 0
        has_double_colon = .false.
        is_all = .false.
        is_none = .false.
        name_count = 0
        allocate (character(len=100) :: import_names(0))

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_text = to_lower(token%text)
                if (trim(lowered_text) == "all") then
                    is_all = .true.
                    token = parser%consume()
                else if (trim(lowered_text) == "none") then
                    is_none = .true.
                    token = parser%consume()
                end if
            end if
        else if (token%kind == TK_OPERATOR .and. token%text == "::") then
            has_double_colon = .true.
            token = parser%consume()
        end if

        if (.not. is_all .and. .not. is_none) then
            do while (.not. parser%is_at_end())
                token = parser%peek()
                select case (token%kind)
                case (TK_IDENTIFIER, TK_KEYWORD)
                    name_count = name_count + 1
                    block
                        character(len=:), allocatable :: tmp_names(:)
                        integer :: i
                        allocate (character(len=100) :: tmp_names(name_count))
                        do i = 1, name_count - 1
                            tmp_names(i) = import_names(i)
                        end do
                        tmp_names(name_count) = trim(token%text)
                        call move_alloc(tmp_names, import_names)
                    end block
                    token = parser%consume()
                case (TK_OPERATOR)
                    if (trim(token%text) == ",") then
                        token = parser%consume()
                    else
                        exit
                    end if
                case (TK_WHITESPACE)
                    token = parser%consume()
                case default
                    exit
                end select
            end do
        end if

        if (name_count > 0) then
            stmt_index = push_import_statement(arena, import_names(1:name_count), &
                has_double_colon=has_double_colon, &
                line=line, column=column)
        else
            stmt_index = push_import_statement(arena, &
                has_double_colon=has_double_colon, &
                is_all=is_all, is_none=is_none, &
                line=line, column=column)
        end if
    end function parse_import_statement

end module parser_interface_import_module
