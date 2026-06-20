program test_inline_instantiation_consumption
    use lexer_core, only: tokenize_core, token_t
    use parser_inline_instantiation_module, only: consume_inline_instantiation
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_token_views_module, only: token_view_t, build_token_view
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== inline instantiation consumption tests ==='

    if (.not. test_balanced_without_view()) all_passed = .false.
    if (.not. test_balanced_with_view()) all_passed = .false.
    if (.not. test_unbalanced_reports_error()) all_passed = .false.
    if (.not. test_caret_without_view()) all_passed = .false.
    if (.not. test_caret_with_view()) all_passed = .false.
    if (.not. test_caret_stops_before_args()) all_passed = .false.
    if (.not. test_caret_unbalanced_reports_error()) all_passed = .false.
    if (.not. test_caret_without_paren_reports_error()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All inline instantiation consumption tests passed!'
        stop 0
    end if

    print *, 'Some inline instantiation consumption tests failed!'
    stop 1

contains

    logical function test_balanced_without_view()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        character(len=:), allocatable :: inst_text

        test_balanced_without_view = .true.

        call tokenize_core('{T{U}}', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (.not. allocated(inst_text)) then
            print *, '  FAIL: Expected allocated instantiation text (no view)'
            test_balanced_without_view = .false.
            return
        end if

        if (inst_text /= '{T{U}}') then
            print *, '  FAIL: Unexpected instantiation text (no view): ', inst_text
            test_balanced_without_view = .false.
            return
        end if
    end function test_balanced_without_view

    logical function test_balanced_with_view()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_view_t) :: view
        character(len=:), allocatable :: inst_text

        test_balanced_with_view = .true.

        call tokenize_core('{T{U}}', tokens)
        parser = create_parser_state(tokens)
        call build_token_view(view, parser)

        call consume_inline_instantiation(parser, inst_text, view)

        if (.not. allocated(inst_text)) then
            print *, '  FAIL: Expected allocated instantiation text (with view)'
            test_balanced_with_view = .false.
            return
        end if

        if (inst_text /= '{T{U}}') then
            print *, '  FAIL: Unexpected instantiation text (with view): ', inst_text
            test_balanced_with_view = .false.
            return
        end if
    end function test_balanced_with_view

    logical function test_unbalanced_reports_error()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        character(len=:), allocatable :: inst_text

        test_unbalanced_reports_error = .true.

        call tokenize_core('{T{U}', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (allocated(inst_text)) then
            print *, '  FAIL: Expected instantiation text to be deallocated on error'
            test_unbalanced_reports_error = .false.
            return
        end if

        if (.not. parser%has_errors()) then
            print *, '  FAIL: Expected an error for unbalanced braces'
            test_unbalanced_reports_error = .false.
            return
        end if
    end function test_unbalanced_reports_error

    logical function test_caret_without_view()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        character(len=:), allocatable :: inst_text

        test_caret_without_view = .true.

        call tokenize_core('^(integer)', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (.not. allocated(inst_text)) then
            print *, '  FAIL: Expected allocated caret text (no view)'
            test_caret_without_view = .false.
            return
        end if

        if (inst_text /= '^(integer)') then
            print *, '  FAIL: Unexpected caret text (no view): ', inst_text
            test_caret_without_view = .false.
            return
        end if
    end function test_caret_without_view

    logical function test_caret_with_view()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_view_t) :: view
        character(len=:), allocatable :: inst_text

        test_caret_with_view = .true.

        call tokenize_core('^(real)', tokens)
        parser = create_parser_state(tokens)
        call build_token_view(view, parser)

        call consume_inline_instantiation(parser, inst_text, view)

        if (.not. allocated(inst_text)) then
            print *, '  FAIL: Expected allocated caret text (with view)'
            test_caret_with_view = .false.
            return
        end if

        if (inst_text /= '^(real)') then
            print *, '  FAIL: Unexpected caret text (with view): ', inst_text
            test_caret_with_view = .false.
            return
        end if
    end function test_caret_with_view

    logical function test_caret_stops_before_args()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(token_t) :: remaining
        character(len=:), allocatable :: inst_text

        test_caret_stops_before_args = .true.

        ! Caret consumes only the type list; the actual-arg group stays.
        call tokenize_core('^(integer)(a, b)', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (.not. allocated(inst_text)) then
            print *, '  FAIL: Expected allocated caret text before args'
            test_caret_stops_before_args = .false.
            return
        end if

        if (inst_text /= '^(integer)') then
            print *, '  FAIL: Caret consumed past type list: ', inst_text
            test_caret_stops_before_args = .false.
            return
        end if

        ! The remaining ( a, b ) must still be available to the caller.
        remaining = parser%peek()
        if (remaining%text /= '(') then
            print *, '  FAIL: Expected ( to remain after caret type list'
            test_caret_stops_before_args = .false.
            return
        end if
    end function test_caret_stops_before_args

    logical function test_caret_unbalanced_reports_error()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        character(len=:), allocatable :: inst_text

        test_caret_unbalanced_reports_error = .true.

        call tokenize_core('^(integer', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (allocated(inst_text)) then
            print *, '  FAIL: Expected caret text deallocated on unbalanced parens'
            test_caret_unbalanced_reports_error = .false.
            return
        end if

        if (.not. parser%has_errors()) then
            print *, '  FAIL: Expected an error for unbalanced caret parens'
            test_caret_unbalanced_reports_error = .false.
            return
        end if
    end function test_caret_unbalanced_reports_error

    logical function test_caret_without_paren_reports_error()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        character(len=:), allocatable :: inst_text

        test_caret_without_paren_reports_error = .true.

        call tokenize_core('^ integer', tokens)
        parser = create_parser_state(tokens)

        call consume_inline_instantiation(parser, inst_text)

        if (allocated(inst_text)) then
            print *, '  FAIL: Expected no text when caret not followed by ('
            test_caret_without_paren_reports_error = .false.
            return
        end if

        if (.not. parser%has_errors()) then
            print *, '  FAIL: Expected an error when caret not followed by ('
            test_caret_without_paren_reports_error = .false.
            return
        end if
    end function test_caret_without_paren_reports_error

end program test_inline_instantiation_consumption
