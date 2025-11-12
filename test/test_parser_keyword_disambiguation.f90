program test_parser_keyword_disambiguation
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    implicit none

    call test_legacy_implicit_statement_detection()
    call test_identifier_assignment_detection()

    print *, 'PASS: parser keyword disambiguation honors implicit statements'

contains

    subroutine test_legacy_implicit_statement_detection()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        logical :: as_identifier

        source = '      implicit real (a-h)' // new_line('a') // '      end'
        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (as_identifier) then
            write (error_unit, '(A)') 'FAIL: legacy IMPLICIT statement parsed as identifier'
            error stop 1
        end if
    end subroutine test_legacy_implicit_statement_detection

    subroutine test_identifier_assignment_detection()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        logical :: as_identifier

        source = 'implicit = 5' // new_line('a') // 'end'
        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') 'FAIL: identifier IMPLICIT assignment was not preserved'
            error stop 1
        end if
    end subroutine test_identifier_assignment_detection

end program test_parser_keyword_disambiguation
