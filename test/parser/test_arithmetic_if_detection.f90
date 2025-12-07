program test_arithmetic_if_detection
    ! Unit test for arithmetic IF detection edge cases
    ! Tests is_arithmetic_if() function with various token patterns
    !
    ! ISO/IEC 1539-1:2018 Reference:
    ! - Annex B.3 item 1: Arithmetic IF is a deleted feature from Fortran 2008
    ! - Section 11.1.8: IF construct and statement (standard block IF)
    !
    ! fortfront accepts arithmetic IF for legacy compatibility and transforms
    ! it to standard-conforming IF/ELSEIF/ELSE block constructs per ISO 2018.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core, TK_KEYWORD, TK_OPERATOR
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_arithmetic_if_module, only: is_arithmetic_if
    implicit none

    integer :: passed, total

    passed = 0
    total = 0

    print *, "=== Arithmetic IF Detection Unit Tests ==="
    print *, ""
    print *, "ISO/IEC 1539-1:2018 Annex B.3 (deleted feature from F2008)"
    print *, "fortfront transforms arithmetic IF to block IF per section 11.1.8"
    print *, ""

    ! Test basic valid pattern
    call test_detection("10, 20, 30", .true., &
        "basic triple label pattern")

    ! Test with extra whitespace
    call test_detection("  10  ,  20  ,  30  ", .true., &
        "whitespace around labels")

    ! Test minimal whitespace
    call test_detection("1,2,3", .true., &
        "minimal spacing labels")

    ! Test with single digit labels
    call test_detection("1, 2, 3", .true., &
        "single digit labels")

    ! Test with larger labels
    call test_detection("100, 200, 300", .true., &
        "three digit labels")

    ! Test with mixed label sizes
    call test_detection("1, 20, 300", .true., &
        "mixed size labels")

    ! Invalid patterns (should NOT be arithmetic IF)

    ! Two labels only
    call test_detection("10, 20", .false., &
        "only two labels")

    ! Four labels
    call test_detection("10, 20, 30, 40", .false., &
        "four labels")

    ! Single label
    call test_detection("10", .false., &
        "single label")

    ! Missing comma
    call test_detection("10 20, 30", .false., &
        "missing first comma")

    ! Expression not label pattern
    call test_detection("x + 1", .false., &
        "expression not labels")

    ! THEN keyword (block IF)
    call test_detection("then", .false., &
        "then keyword indicates block IF")

    print *, ""
    print *, "=== Results: ", passed, " / ", total, " passed ==="

    if (passed == total) then
        print *, "PASS: All arithmetic IF detection tests passed"
    else
        write (error_unit, '(A)') "FAIL: Some arithmetic IF detection tests failed"
        error stop 1
    end if

contains

    subroutine test_detection(after_paren, expected, description)
        character(len=*), intent(in) :: after_paren
        logical, intent(in) :: expected
        character(len=*), intent(in) :: description

        character(len=:), allocatable :: source
        type(token_t), allocatable, target :: tokens(:)
        type(parser_state_t) :: parser
        logical :: result_val
        integer :: i, paren_depth

        total = total + 1

        ! Create minimal source with IF and expression
        source = "if (x) " // after_paren

        call tokenize_core(source, tokens)

        ! Create parser state with proper target attribute
        parser = create_parser_state(tokens)

        ! Advance past if keyword
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "if") then
                parser%current_token = i + 1
                exit
            end if
        end do

        ! Find position after closing paren
        paren_depth = 0
        do while (parser%current_token <= size(tokens))
            if (tokens(parser%current_token)%kind == TK_OPERATOR) then
                if (tokens(parser%current_token)%text == "(") then
                    paren_depth = paren_depth + 1
                else if (tokens(parser%current_token)%text == ")") then
                    paren_depth = paren_depth - 1
                    if (paren_depth == 0) then
                        parser%current_token = parser%current_token + 1
                        exit
                    end if
                end if
            end if
            parser%current_token = parser%current_token + 1
        end do

        result_val = is_arithmetic_if(parser)

        if (result_val .eqv. expected) then
            passed = passed + 1
            print *, "  PASS: ", trim(description)
        else
            print *, "  FAIL: ", trim(description)
            print *, "    Expected: ", expected, " Got: ", result_val
        end if
    end subroutine test_detection

end program test_arithmetic_if_detection
