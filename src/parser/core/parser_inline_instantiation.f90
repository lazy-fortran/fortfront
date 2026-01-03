module parser_inline_instantiation_module
    use lexer_core, only: token_t
    use lexer_token_types, only: TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use parser_token_views_module, only: token_view_t, view_peek_token, &
                                         view_consume_token
    implicit none
    private

    public :: consume_inline_instantiation

contains

    subroutine consume_inline_instantiation(parser, inst_text, view)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: inst_text
        type(token_view_t), intent(in), optional :: view

        if (present(view)) then
            call consume_inline_instantiation_core(parser, inst_text, view)
        else
            call consume_inline_instantiation_core(parser, inst_text)
        end if
    end subroutine consume_inline_instantiation

    subroutine consume_inline_instantiation_core(parser, inst_text, view)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: inst_text
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token
        type(token_t) :: start_token
        integer :: nesting

        if (allocated(inst_text)) deallocate (inst_text)

        if (present(view)) then
            token = view_peek_token(view, parser)
        else
            token = parser%peek()
        end if

        if (.not. (token%kind == TK_OPERATOR .and. token%text == "{")) then
            return
        end if

        start_token = token
        nesting = 0
        do while (.not. parser%is_at_end())
            if (present(view)) then
                token = view_consume_token(view, parser)
            else
                token = parser%consume()
            end if

            if (token%kind == TK_OPERATOR) then
                if (token%text == "{") nesting = nesting + 1
                if (token%text == "}") nesting = nesting - 1
            end if

            if (.not. allocated(inst_text)) then
                inst_text = token%text
            else
                inst_text = inst_text // token%text
            end if

            if (nesting == 0) exit
        end do

        if (nesting /= 0) then
            if (allocated(inst_text)) deallocate (inst_text)
            call parser%errors%add_error_with_token( &
                "Unbalanced inline instantiation braces: expected } "// &
                "before end of file", &
                start_token, suggestion="Add a matching } to close the instantiation")
        end if
    end subroutine consume_inline_instantiation_core

end module parser_inline_instantiation_module
