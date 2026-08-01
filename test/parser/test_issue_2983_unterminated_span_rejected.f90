program test_issue_2983_unterminated_span_rejected
    ! fortfront #2983.
    !
    ! The block-construct span scanner locates the end of a construct by
    ! matching its terminator. When it cannot find one it used to run to the
    ! end of the token array and return that as the span, so every statement
    ! after the construct was absorbed and silently vanished from the AST.
    ! That is the mechanism behind seven known defects (#2928, the bare-END
    ! procedure span, and #2966/#2967/#2972/#2974/#2977).
    !
    ! The fix keeps one canonical mechanism rather than adding a second: the
    ! pre-parse construct-terminator validator already maintains a stack of
    ! open constructs, but never checked that the stack was empty at end of
    ! input, and tracked only DO, BLOCK and INTERFACE. It now also tracks the
    ! block form of IF, SELECT and ASSOCIATE, and reports whatever is still
    ! open when the token stream ends.
    !
    ! This test pins both directions: an unterminated construct is rejected
    ! with a diagnostic naming the terminator it wants, and constructs that ARE
    ! terminated -- including one that is the last statement of a procedure,
    ! where the terminator sits immediately before END SUBROUTINE -- still
    ! parse.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: TK_KEYWORD
    use string_utils_mod, only: to_lower
    use parser_block_statement_utils_module, only: locate_block_statement_end
    implicit none

    integer :: failures

    failures = 0

    call expect_rejected('unterminated DO in a module procedure', 'END DO', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i'//nl()// &
        '      do i = 1, 3'//nl()// &
        '         print *, i'//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    call expect_rejected('unterminated IF in a module procedure', 'END IF', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i'//nl()// &
        '      i = 1'//nl()// &
        '      if (i == 1) then'//nl()// &
        '         i = 2'//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    ! Negative controls: every one of these is accepted by
    ! "gfortran -fsyntax-only" and must keep parsing.
    call expect_accepted('DO as the last statement of a procedure', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i'//nl()// &
        '      do i = 1, 3'//nl()// &
        '         print *, i'//nl()// &
        '      end do'//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    call expect_accepted('nested DO nest followed by a statement', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i, j'//nl()// &
        '      do j = 1, 2'//nl()// &
        '         do i = 1, 3'//nl()// &
        '         end do'//nl()// &
        '      end do'//nl()// &
        "      print *, 'a'"//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    call expect_accepted('upper-case DO ... END DO', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   SUBROUTINE work()'//nl()// &
        '      INTEGER :: i'//nl()// &
        '      DO i = 1, 3'//nl()// &
        '         PRINT *, i'//nl()// &
        '      END DO'//nl()// &
        '   END SUBROUTINE work'//nl()// &
        'end module m', failures)

    call expect_accepted('named IF construct', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i'//nl()// &
        '      i = 0'//nl()// &
        '      check: if (i == 0) then'//nl()// &
        '         i = 1'//nl()// &
        '      end if check'//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    call expect_accepted('one-line IF needs no terminator', &
        'module m'//nl()// &
        'contains'//nl()// &
        '   subroutine work()'//nl()// &
        '      integer :: i'//nl()// &
        '      i = 0'//nl()// &
        '      if (i == 0) i = 1'//nl()// &
        "      print *, 'a'"//nl()// &
        '   end subroutine work'//nl()// &
        'end module m', failures)

    ! The scanner invariant itself, exercised directly. It is meant to be
    ! unreachable through the parser -- the validator above rejects
    ! unterminated sources first -- so it is pinned at the unit level.
    call check_span_scanner_invariant(failures)

    if (failures > 0) then
        print *, 'FAIL: ', failures, ' unterminated-span checks'
        error stop 1
    end if
    print *, 'PASS: unterminated construct spans are reported, not swallowed'

contains

    function nl() result(c)
        character(len=1) :: c
        c = new_line('a')
    end function nl

    ! parse_tokens takes error_msg as character(len=*), so the buffer has to
    ! be long enough for the diagnostic; a deferred-length actual argument
    ! sized by lex_source would silently truncate it to zero characters.
    subroutine parse_source(src, error_msg)
        character(len=*), intent(in) :: src
        character(len=1024), intent(out) :: error_msg
        character(:), allocatable :: lex_error
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        error_msg = ''
        arena = create_ast_arena()
        call lex_source(src, tokens, lex_error)
        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                error_msg = lex_error
                return
            end if
        end if
        call parse_tokens(tokens, arena, prog_index, error_msg)
    end subroutine parse_source

    ! locate_block_statement_end reports whether it accounted for the tokens
    ! it scanned: .false. when it matched the terminator, .true. when it ran to
    ! the end of the array with the construct still open.
    subroutine check_span_scanner_invariant(failures)
        integer, intent(inout) :: failures
        type(token_t), allocatable :: tokens(:)
        character(:), allocatable :: lex_error
        integer :: i, do_pos, span_end
        logical :: unaccounted

        call lex_source('do i = 1, 3'//nl()//'   print *, i'//nl()// &
            'end do'//nl()//'print *, 0'//nl(), tokens, lex_error)
        do_pos = 0
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle
            if (trim(to_lower(tokens(i)%text)) == 'do') then
                do_pos = i
                exit
            end if
        end do
        if (do_pos == 0) then
            print *, 'FAIL: could not locate DO token in fixture'
            failures = failures + 1
            return
        end if
        span_end = locate_block_statement_end(tokens, do_pos, 'do', unaccounted)
        if (unaccounted) then
            print *, 'FAIL: scanner reported terminated DO as unaccounted'
            failures = failures + 1
        end if
        if (span_end >= size(tokens)) then
            print *, 'FAIL: terminated DO span reached the end of the array'
            failures = failures + 1
        end if

        call lex_source('do i = 1, 3'//nl()//'   print *, i'//nl(), tokens, &
            lex_error)
        do_pos = 0
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle
            if (trim(to_lower(tokens(i)%text)) == 'do') then
                do_pos = i
                exit
            end if
        end do
        if (do_pos == 0) then
            print *, 'FAIL: could not locate DO token in fixture'
            failures = failures + 1
            return
        end if
        span_end = locate_block_statement_end(tokens, do_pos, 'do', unaccounted)
        if (.not. unaccounted) then
            print *, 'FAIL: scanner silently returned a span for an '// &
                'unterminated DO'
            failures = failures + 1
        end if
    end subroutine check_span_scanner_invariant

    subroutine expect_rejected(label, wanted, src, failures)
        character(len=*), intent(in) :: label, wanted, src
        integer, intent(inout) :: failures
        character(len=1024) :: error_msg

        call parse_source(src, error_msg)
        if (len_trim(error_msg) == 0) then
            print *, 'FAIL: accepted silently, source would be dropped: ', label
            failures = failures + 1
            return
        end if
        if (index(error_msg, 'Expecting '//wanted) == 0) then
            print *, 'FAIL: rejected without naming the missing '// &
                wanted//' terminator: ', label
            print *, '      got: ', trim(error_msg)
            failures = failures + 1
        end if
    end subroutine expect_rejected

    subroutine expect_accepted(label, src, failures)
        character(len=*), intent(in) :: label, src
        integer, intent(inout) :: failures
        character(len=1024) :: error_msg

        call parse_source(src, error_msg)
        if (len_trim(error_msg) /= 0) then
            print *, 'FAIL: valid source rejected: ', label
            print *, '      got: ', trim(error_msg)
            failures = failures + 1
        end if
    end subroutine expect_accepted

end program test_issue_2983_unterminated_span_rejected
