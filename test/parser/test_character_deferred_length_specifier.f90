program test_character_deferred_length_specifier
    use lexer_api, only: lex_source
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_type_specifier, type_specifier_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    call check_spec("character(:), allocatable :: text", .true., ":", &
        "character(len=:)")
    call check_spec("character(len=:), allocatable :: text", .true., ":", &
        "character(len=:)")
    call check_spec("character(len=12) :: text", .true., "12", &
        "character(len=12)")
    call check_spec("character(len=*) :: text", .true., "*", &
        "character(len=*)")
    call check_spec("character*8 :: text", .true., "8", &
        "character(len=8)")

    print *, "PASS: character length selectors are preserved"

contains

    subroutine check_spec(source, expected_has_length, expected_length, &
            expected_type_name)
        character(len=*), intent(in) :: source
        logical, intent(in) :: expected_has_length
        character(len=*), intent(in) :: expected_length
        character(len=*), intent(in) :: expected_type_name
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(parser_state_t) :: parser
        type(type_specifier_t) :: type_spec
        type(ast_arena_t) :: arena

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        type_spec = parse_type_specifier(parser, arena)

        if (type_spec%has_character_length .neqv. expected_has_length) then
            print *, "FAIL: character length presence for:", trim(source)
            error stop 1
        end if
        if (.not. allocated(type_spec%character_length_expr)) then
            print *, "FAIL: character length expression missing for:", trim(source)
            error stop 1
        end if
        if (trim(type_spec%character_length_expr) /= expected_length) then
            print *, "FAIL: character length expression for:", trim(source)
            print *, "      got:", trim(type_spec%character_length_expr)
            error stop 1
        end if
        if (trim(type_spec%type_name) /= expected_type_name) then
            print *, "FAIL: character type name for:", trim(source)
            print *, "      got:", trim(type_spec%type_name)
            error stop 1
        end if
    end subroutine check_spec

end program test_character_deferred_length_specifier
