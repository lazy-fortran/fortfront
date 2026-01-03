module parser_template_blocks_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_IDENTIFIER, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_template_block
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_module_structures_module, only: parse_module_body
    implicit none
    private

    public :: parse_template_block

contains

    function parse_template_block(parser, arena) result(template_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: template_index
        type(parser_prefix_buffer_t) :: prefix_buffer
        type(token_t) :: token
        character(len=:), allocatable :: template_name
        character(len=:), allocatable :: parameter_names(:)
        integer :: line, column
        integer, allocatable :: declaration_indices(:), procedure_indices(:)
        logical :: has_contains, in_contains_section

        token = parser%consume()
        line = token%line
        column = token%column

        template_name = "unnamed_template"
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            template_name = token%text
        end if

        call parse_optional_template_parameters(parser, parameter_names)

        allocate (declaration_indices(0))
        allocate (procedure_indices(0))
        has_contains = .false.
        in_contains_section = .false.

        call parse_module_body(parser, arena, prefix_buffer, &
                               has_contains, in_contains_section, &
                               declaration_indices, procedure_indices, &
                               compact_end_keyword="endtemplate", &
                               paired_end_keyword="template")

        template_index = push_template_block(arena, template_name, &
                                             parameter_names, &
                                             declaration_indices, &
                                             procedure_indices, &
                                             has_contains, line, column)
    end function parse_template_block

    subroutine parse_optional_template_parameters(parser, parameter_names)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: parameter_names(:)
        type(token_t) :: token
        character(len=:), allocatable :: current

        allocate (character(len=1) :: parameter_names(0))

        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "(")) then
            return
        end if

        token = parser%consume()

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                token = parser%consume()
                current = token%text
                parameter_names = [parameter_names, current]
            else
                token = parser%consume()
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            end if
        end do
    end subroutine parse_optional_template_parameters

end module parser_template_blocks_module
