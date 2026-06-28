program test_parser_keyword_disambiguation
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core, to_lower
    use lexer_token_types, only: TK_KEYWORD, TK_OPERATOR
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    implicit none

    call test_legacy_implicit_statement_detection()
    call test_identifier_assignment_detection()
    call test_stop_assignment_detection()
    call test_program_assignment_detection()
    call test_program_assignment_in_context()
    call test_module_assignment_in_context()
    call test_common_assignment_detection()

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
            write (error_unit, '(A)') &
                'FAIL: legacy IMPLICIT statement parsed as identifier'
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
            write (error_unit, '(A)') &
                'FAIL: identifier IMPLICIT assignment was not preserved'
            error stop 1
        end if
    end subroutine test_identifier_assignment_detection

    subroutine test_stop_assignment_detection()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        logical :: as_identifier

        source = 'stop = 1' // new_line('a') // 'end'
        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') &
                'FAIL: identifier STOP assignment was not preserved'
            error stop 1
        end if
    end subroutine test_stop_assignment_detection

    subroutine test_program_assignment_detection()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        logical :: as_identifier

        source = 'program = 10' // new_line('a') // 'end'
        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') &
                'FAIL: identifier PROGRAM assignment was not preserved'
            error stop 1
        end if
    end subroutine test_program_assignment_detection

    subroutine test_program_assignment_in_context()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: current_token, first_token, next_token
        logical :: found_assignment, as_identifier

        source = 'program demo_program_kw' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            '    integer :: program' // new_line('a') // &
            '    program = 10' // new_line('a') // &
            '    print *, program' // new_line('a') // &
            'end program demo_program_kw'

        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        found_assignment = .false.

        do while (.not. parser%is_at_end())
            current_token = parser%peek()
            if (current_token%kind == TK_KEYWORD) then
                if (trim(to_lower(current_token%text)) == 'program') then
                    next_token = parser%get_token_at_index(parser%current_token + 1)
                    if (next_token%kind == TK_OPERATOR .and. trim(next_token%text) &
                        == '=') then
                        found_assignment = .true.
                        exit
                    end if
                end if
            end if
            current_token = parser%consume()
        end do

        if (.not. found_assignment) then
            write (error_unit, '(A)') 'FAIL: failed to locate program assignment token'
            error stop 1
        end if

        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') &
                'FAIL: keyword disambiguation failed inside program body'
            error stop 1
        end if
    end subroutine test_program_assignment_in_context

    subroutine test_module_assignment_in_context()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: current_token, next_token, first_token
        logical :: found_assignment, as_identifier

        source = 'program demo_module_kw' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            '    integer :: module' // new_line('a') // &
            '    module = 3' // new_line('a') // &
            '    print *, module' // new_line('a') // &
            'end program demo_module_kw'

        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        found_assignment = .false.

        do while (.not. parser%is_at_end())
            current_token = parser%peek()
            if (current_token%kind == TK_KEYWORD) then
                if (trim(to_lower(current_token%text)) == 'module') then
                    next_token = parser%get_token_at_index(parser%current_token + 1)
                    if (next_token%kind == TK_OPERATOR .and. &
                        trim(next_token%text) == '=') then
                        found_assignment = .true.
                        exit
                    end if
                end if
            end if
            current_token = parser%consume()
        end do

        if (.not. found_assignment) then
            write (error_unit, '(A)') 'FAIL: failed to locate module assignment token'
            error stop 1
        end if

        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') &
                'FAIL: keyword disambiguation failed for module identifier'
            error stop 1
        end if
    end subroutine test_module_assignment_in_context

    subroutine test_common_assignment_detection()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        logical :: as_identifier

        source = 'common = 1' // new_line('a') // 'end'
        call tokenize_core(source, tokens)
        parser = create_parser_state(tokens)
        first_token = parser%peek()
        as_identifier = keyword_should_parse_as_identifier(first_token, parser)

        if (.not. as_identifier) then
            write (error_unit, '(A)') &
                'FAIL: identifier COMMON assignment was not preserved'
            error stop 1
        end if
    end subroutine test_common_assignment_detection

end program test_parser_keyword_disambiguation
