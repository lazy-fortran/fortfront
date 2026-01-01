module frontend_statement_processing
    ! Statement-level parsing and processing functionality
    ! Handles parsing of all statements into a program structure

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_WHITESPACE
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use frontend_statement_contains_section, only: is_structural_contains, &
                                                   parse_implicit_contains_section
    use frontend_statement_spec_section, only: convert_statement_function_if_needed, &
                                               update_spec_section_state
    use frontend_statement_boundary, only: find_statement_boundary
    use frontend_statement_token_parsing, only: parse_explicit_program_unit, &
                                                process_comment_statement, &
                                                process_regular_statement, &
                                                is_prefix_only_statement
    use frontend_program_structure, only: create_final_program_structure, &
                                          handle_multiple_program_units, &
                                          should_include_program_unit, &
                                          is_empty_main_program

    implicit none
    private

    ! Public statement processing interface
    public :: parse_all_statements, parse_explicit_program_unit
    public :: process_comment_statement, process_regular_statement

    ! Re-export from other modules
    public :: create_final_program_structure, handle_multiple_program_units
    public :: should_include_program_unit, is_empty_main_program
    public :: find_statement_boundary

contains

    ! Parse all statements into a program block
    function parse_all_statements(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index

        integer, allocatable :: body_indices(:)
        type(parser_prefix_buffer_t) :: prefix_buffer
        integer :: i, stmt_start, stmt_end, stmt_index, stmt_count
        integer :: merged_start, merged_end, look_ahead
        logical :: in_spec_section
        integer, allocatable :: declaration_indices(:)

        allocate (body_indices(0))
        allocate (declaration_indices(0))
        stmt_count = 0
        i = 1
        in_spec_section = .true.

        ! Process all statements
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            call find_statement_boundary(tokens, i, stmt_start, stmt_end)

            ! Check for structural contains keyword (implicit main program)
            if (is_structural_contains(tokens, stmt_start, stmt_end)) then
                call parse_implicit_contains_section(tokens, stmt_end + 1, arena, &
                                                     body_indices, i)
                exit
            end if

            ! NOTE: We do NOT check for bare end here because:
            ! 1. For standard Fortran round-trips, bare end should be preserved as-is
            ! 2. The contains section parser handles end detection internally
            ! 3. Removing bare end detection here prevents premature exit for
            !    files like f(x) = x + 1 followed by bare end, which should round-trip

            if (is_prefix_only_statement(tokens, stmt_start, stmt_end)) then
                look_ahead = stmt_end + 1
                do while (look_ahead <= size(tokens))
                    if (tokens(look_ahead)%kind == TK_WHITESPACE .or. &
                        tokens(look_ahead)%kind == TK_NEWLINE) then
                        look_ahead = look_ahead + 1
                    else
                        exit
                    end if
                end do
                if (look_ahead <= size(tokens)) then
                    call find_statement_boundary(tokens, look_ahead, merged_start, &
                                                 merged_end)
                    if (merged_start == look_ahead) then
                        if (tokens(merged_start)%kind == TK_KEYWORD) then
                            if (tokens(merged_start)%text == "function" .or. &
                                tokens(merged_start)%text == "subroutine") then
                                stmt_end = merged_end
                            end if
                        end if
                    end if
                end if
            end if

            if (tokens(stmt_start)%kind == TK_COMMENT) then
                call process_comment_statement(tokens, stmt_start, arena, &
                                               prefix_buffer, stmt_index, body_indices)
            else
                call process_regular_statement(tokens, stmt_start, stmt_end, arena, &
                                               prefix_buffer, stmt_index, body_indices)
            end if

            if (stmt_index > 0) then
                if (in_spec_section) then
                    call convert_statement_function_if_needed(arena, stmt_index, &
                                                              declaration_indices)
                end if
                call update_spec_section_state(arena, stmt_index, in_spec_section, &
                                               declaration_indices)
                stmt_count = stmt_count + 1
            end if
            i = stmt_end + 1
        end do

        ! Create final program structure
        call create_final_program_structure(arena, body_indices, stmt_count, &
                                            prog_index)
    end function parse_all_statements

end module frontend_statement_processing
