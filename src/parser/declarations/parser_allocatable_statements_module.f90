module parser_allocatable_statements_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        & TK_WHITESPACE, TK_COMMENT, TK_NEWLINE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use parser_declaration_attributes_module, only: parse_array_dimensions
    implicit none
    private

    public :: parse_allocatable_statement

contains

    logical function parse_allocatable_statement(parser, arena) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: lowered_keyword

        success = .false.
        if (parser%is_at_end()) return

        call skip_trivia(parser)

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_keyword = to_lower(token%text)
                if (trim(lowered_keyword) == "allocatable") then
                    token = parser%consume()
                    call skip_trivia(parser)
                end if
            end if
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
                call skip_trivia(parser)
            end if
        end if

        do
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                call skip_trivia(parser)
                cycle
            end if

            if (token%kind == TK_NEWLINE) exit

            if (.not. token_is_identifier(token)) exit

            if (parse_single_allocatable_var(parser, arena)) then
                success = .true.
            end if

            call skip_trivia(parser)
        end do
    end function parse_allocatable_statement

    logical function parse_single_allocatable_var(parser, arena) result(applied)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: var_name
        integer, allocatable :: dimension_indices(:)
        logical :: has_dimensions

        applied = .false.
        if (parser%is_at_end()) return

        token = parser%peek()
        if (.not. token_is_identifier(token)) return
        token = parser%consume()
        var_name = adjustl(trim(token%text))

        call skip_trivia(parser)

        has_dimensions = .false.
        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                call parse_array_dimensions(parser, arena, dimension_indices)
                has_dimensions = .true.
            end if
        end if

        if (.not. allocated(dimension_indices)) then
            allocate (dimension_indices(0))
        end if

        applied = apply_allocatable_to_variable(arena, var_name, dimension_indices, &
            has_dimensions)
        if (allocated(dimension_indices)) deallocate (dimension_indices)
    end function parse_single_allocatable_var

    subroutine skip_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_trivia

    logical function token_is_identifier(token) result(is_ident)
        type(token_t), intent(in) :: token
        is_ident = (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD)
    end function token_is_identifier

    logical function apply_allocatable_to_variable(arena, name, dimension_indices, &
            has_dimensions) result(applied)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension_indices(:)
        logical, intent(in) :: has_dimensions
        character(len=:), allocatable :: target, decl_name
        integer :: idx

        applied = .false.
        target = to_lower(adjustl(trim(name)))

        do idx = arena%size, 1, -1
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
                type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    if (apply_allocatable_multi(arena, idx, decl, target, &
                        dimension_indices, has_dimensions)) then
                        applied = .true.
                        return
                    end if
                end if
                if (allocated(decl%var_name)) then
                    decl_name = to_lower(trim(decl%var_name))
                    if (decl_name == target) then
                        decl%is_allocatable = .true.
                        if (has_dimensions) then
                            decl%is_array = .true.
                            call set_declaration_dimensions(decl, dimension_indices)
                        end if
                        arena%entries(idx)%node = decl
                        applied = .true.
                        return
                    end if
                end if
            end select
        end do
    end function apply_allocatable_to_variable

    logical function apply_allocatable_multi(arena, decl_index, decl, target, &
            dimension_indices, has_dimensions) &
            result(updated)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        type(declaration_node), intent(inout) :: decl
        character(len=*), intent(in) :: target
        integer, intent(in) :: dimension_indices(:)
        logical, intent(in) :: has_dimensions
        integer :: i
        character(len=:), allocatable :: decl_name

        updated = .false.
        if (.not. allocated(decl%var_names)) return

        do i = 1, size(decl%var_names)
            decl_name = to_lower(trim(decl%var_names(i)))
            if (decl_name == target) then
                decl%is_allocatable = .true.
                if (has_dimensions) then
                    decl%is_array = .true.
                    call set_declaration_dimensions(decl, dimension_indices)
                end if
                arena%entries(decl_index)%node = decl
                updated = .true.
                return
            end if
        end do
    end function apply_allocatable_multi

    subroutine set_declaration_dimensions(decl, dimension_indices)
        type(declaration_node), intent(inout) :: decl
        integer, intent(in) :: dimension_indices(:)

        if (allocated(decl%dimension_indices)) then
            deallocate (decl%dimension_indices)
        end if
        if (size(dimension_indices) > 0) then
            allocate (decl%dimension_indices(size(dimension_indices)))
            decl%dimension_indices = dimension_indices
        else
            allocate (decl%dimension_indices(0))
        end if
    end subroutine set_declaration_dimensions

end module parser_allocatable_statements_module
