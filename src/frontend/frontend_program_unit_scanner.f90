module frontend_program_unit_scanner
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_WHITESPACE, &
                          to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: entry_node
    use frontend_program_units, only: parse_program_unit

    implicit none
    private

    public :: detect_explicit_program_unit, is_inside_module, is_program_unit_start
    public :: unit_has_meaningful_content, should_process_unit, process_program_unit
    public :: find_program_unit_boundary, procedure_has_entry
    public :: find_next_nontrivial_index, token_precedes_identifier
    public :: token_requires_identifier_after, token_follows_identifier_context
    public :: keyword_can_be_identifier, token_is_block_keyword

contains

    include 'frontend_program_unit_scanner_part1.inc'
    include 'frontend_program_unit_scanner_part2.inc'
    include 'frontend_program_unit_scanner_part3.inc'

end module frontend_program_unit_scanner
