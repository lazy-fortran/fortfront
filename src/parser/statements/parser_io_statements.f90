module parser_io_statements_module
    ! Parser module for I/O statement types (print, write, read)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: &
        io_specifier_t, inquire_statement_node, backspace_statement_node, &
        rewind_statement_node, endfile_statement_node
    use parser_io_specifiers, only: parse_io_specifier_nodes
    use ast_factory, only: push_print_statement, push_write_statement, &
        push_read_statement, push_format_statement, &
        push_open_statement, push_close_statement, &
        push_inquire_statement, push_backspace_statement, &
        push_rewind_statement, push_endfile_statement, &
        push_io_implied_do
    implicit none
    private

    public :: parse_print_statement, parse_write_statement, parse_read_statement
    public :: parse_format_statement
    public :: parse_open_statement, parse_close_statement
    public :: parse_inquire_statement
    public :: parse_backspace_statement, parse_rewind_statement, parse_endfile_statement

    abstract interface
        pure logical function io_control_specifier_predicate(token)
            import :: token_t
            type(token_t), intent(in) :: token
        end function io_control_specifier_predicate
    end interface

contains

    include 'parser_io_statements_common.inc'
    include 'parser_io_statements_parsers.inc'

end module parser_io_statements_module
