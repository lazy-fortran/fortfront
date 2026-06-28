module parser_intrinsic_statements_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_intrinsic_statement
    implicit none
    private

    public :: parse_intrinsic_statement

contains

    function parse_intrinsic_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer :: line, column
        logical :: has_double_colon
        character(len=64), allocatable :: names(:)
        integer :: count

        stmt_index = 0
        has_double_colon = .false.
        count = 0

        token = parser%consume()
        line = token%line
        column = token%column

        if (.not. allocated(names)) then
            allocate (names(8))
            names = ""
        end if

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
            has_double_colon = .true.
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
                exit
            end if

            token = parser%consume()
            if (len_trim(token%text) == 0) cycle

            count = count + 1
            if (count > size(names)) then
                call grow_name_buffer(names)
            end if
            names(count) = trim(token%text)

            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                cycle
            end if
            exit
        end do

        if (count <= 0) then
            return
        end if

        stmt_index = push_intrinsic_statement(arena, names(1:count), line, column, &
            has_double_colon=has_double_colon)

    contains
        subroutine grow_name_buffer(buffer)
            character(len=64), allocatable, intent(inout) :: buffer(:)
            character(len=64), allocatable :: temp(:)
            integer :: old_size

            old_size = size(buffer)
            allocate (temp(old_size * 2))
            temp = ""
            temp(1:old_size) = buffer
            call move_alloc(temp, buffer)
        end subroutine grow_name_buffer
    end function parse_intrinsic_statement

end module parser_intrinsic_statements_module
